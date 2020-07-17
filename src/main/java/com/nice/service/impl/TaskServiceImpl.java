/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.PaymentMode;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.dto.CashCollectionDTO;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.TaskResponseDto;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TaskMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.PaymentDetails;
import com.nice.model.Task;
import com.nice.repository.OrdersRepository;
import com.nice.repository.TaskHistoryRepository;
import com.nice.repository.TaskRepository;
import com.nice.service.CashcollectionService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.TaskService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Jul-2020
 */
@Service(value = "taskService")
@Transactional(rollbackFor = Throwable.class)
public class TaskServiceImpl implements TaskService {

	@Autowired
	private OrdersService orderService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TaskMapper taskMapper;

	@Autowired
	private TaskRepository taskRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TaskHistoryRepository taskHistoryRepostory;
	
	@Autowired
	private CashcollectionService cashCollectionService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private OrdersRepository ordersRepository;

	@Override
	public Task createTask(final TaskDto taskDto) throws NotFoundException, ValidationException {

		Long deliveryBoyId = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser().getEntityId();
		taskDto.setDeliveryBoyId(deliveryBoyId);
		DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(taskDto.getDeliveryBoyId());
		Orders orders = orderService.getOrderById(taskDto.getOrderId());

		/**
		 * Calculate the admin comission here and also the net amount payable to vendor for the task, this code is only for
		 * regular order, not for replacement or return, for replacement and return the calculation for the same will be
		 * different.
		 *
		 */

		// TODO : Change the admin commission rate and get it from settings.
		Double adminCommisionRate = 2.25d; // this is percentage
		Double orderTotal = orders.getTotalOrderAmount();
		Double deliveryCharge = orders.getDeliveryCharge();
		Double adminCommissionAmt = 0.0d;
		Double vendorPayableAmt = 0.0d;
		if (TaskTypeEnum.DELIVERY.name().equals(taskDto.getTaskType())) {
			adminCommissionAmt = ((orderTotal - deliveryCharge) * adminCommisionRate) / 100;
			vendorPayableAmt = orderTotal - deliveryCharge - adminCommissionAmt;
		}

		/**
		 * This code is synchronized as multiple delivery boys trying to accept the same order for delivery donot end up have
		 * the same order.
		 */
		synchronized (this) {
			/**
			 * Check if the task is not already assigned to delivery person.
			 */
			Long count = taskRepository.countByOrderAndTaskType(orders, taskDto.getTaskType());
			if (count > 0) {
				throw new ValidationException(messageByLocaleService.getMessage("order.already.allocated", null));
			}
			/**
			 * Assign delivery boy to order
			 */
			if (TaskTypeEnum.DELIVERY.name().equals(taskDto.getTaskType()) && orders.getDeliveryBoy() != null) {
				throw new ValidationException(messageByLocaleService.getMessage("order.already.allocated", null));
			} else {
				orders.setDeliveryBoy(deliveryBoy);
				ordersRepository.save(orders);
			}

			/**
			 * Task Details
			 */
			Task task = taskMapper.toEntity(taskDto);
			task.setDeliveryBoy(deliveryBoy);
			task.setOrder(orders);
			task.setStatus(TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue());
			task.setTaskType(taskDto.getTaskType());
			task.setActive(true);
			task.setVendorPayableAmt(vendorPayableAmt);
			task.setAdminCommission(adminCommissionAmt);
			task.setTotalOrderAmount(orderTotal);
			task.setDeliveryCharge(deliveryCharge);
			taskRepository.save(task);

			/**
			 * Save values in task History
			 */
			// saveTaskHistory(task);

			return task;
		}
	}

	/**
	 * @param task
	 */
	// private void saveTaskHistory(final Task task) {
	// TaskHistory taskHistory = new TaskHistory();
	// BeanUtils.copyProperties(task, taskHistory);
	// taskHistory.setOrderId(task.getOrder().getId());
	// taskHistory.setDeliveryBoyId(task.getDeliveryBoy().getId());
	// taskHistory.setId(null);
	// taskHistory.setTask(task);
	// taskHistoryRepostory.save(taskHistory);
	// }

	@Override
	public void changeTaskStatus(final Long taskId, final String taskStatus) throws ValidationException, NotFoundException {
		Optional<Task> optTask = taskRepository.findById(taskId);
		if (!optTask.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage("task.not.found", new Object[] { "Id :" + taskId }));
		}
		Task task = optTask.get();

		/**
		 * Check if the next task status is allowed
		 */
		TaskStatusEnum existingTaskStatus = TaskStatusEnum.getByValue(task.getStatus());
		if (!existingTaskStatus.contains(taskStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed", new Object[] { taskStatus, task.getStatus() }));
		}

		if (taskStatus.equals(TaskStatusEnum.DELIVERED.getStatusValue())) {
			task.setStatus(TaskStatusEnum.DELIVERED.getStatusValue());
			/**
			 * Change the status of order based on the task type, if the task type is replacement, the order is being replaced and
			 * hence the order should be moved to replaced status, else its first time delivery and order will be moved to delivered
			 * status, this would be applicable only if there is replacement in place.
			 */
			orderService.changeStatus(OrderStatusEnum.DELIVERED.getStatusValue(), task.getOrder());

			/**
			 * If its a COD task then make an entry in cash collection for the delivery person
			 */
			if (PaymentMode.COD.name().equals(task.getOrder().getPaymentMode())) {
				CashCollectionDTO cashCollection = new  CashCollectionDTO();
				cashCollection.setAmount( task.getOrder().getTotalOrderAmount());
				cashCollection.setDeliveryboyId(task.getOrder().getDeliveryBoy().getId());
				cashCollection.setOrderId(task.getOrder().getId());
                cashCollection.setTaskId(task.getId());
                cashCollection.setActive(true);
                cashCollectionService.addCashCollection(cashCollection);
			}
		} else if (taskStatus.equals(TaskStatusEnum.ON_THE_WAY.getStatusValue())) {
			task.setStatus(TaskStatusEnum.ON_THE_WAY.getStatusValue());
			/**
			 * Change order status here to Order PickUp.
			 */
			orderService.changeStatus(OrderStatusEnum.CANCELLED.getStatusValue(), task.getOrder());
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.task.status", null));
		}
		taskRepository.save(task);
		// saveTaskHistory(task);
	}

	@Override
	public List<Task> getOrderListForDeliveryBoyBasedOnTaskTypeAndDate(final DeliveryBoy deliveryBoy, final String taskType, final Date date,
			final List<String> status) {
		return taskRepository.findAllByDeliveryBoyAndTaskTypeAndUpdatedAtBetweenAndStatusIgnoreCaseIn(deliveryBoy, taskType,
				CommonUtility.getDateWithoutTime(date), CommonUtility.getTomorrowDateWithoutTime(date), status);
	}

	@Override
	public Integer getOrdersCountAccordingToStatusAndTaskTypeForDeliveryBoy(final DeliveryBoy deliveryBoy, final String status, final String taskType) {
		return taskRepository.countByDeliveryBoyAndStatusAndTaskType(deliveryBoy, status, taskType);
	}

	@Override
	public Integer getOrdersCountAccordingToStatusAndTaskTypeAndDateForDeliveryBoy(final DeliveryBoy deliveryBoy, final String status, final String taskType,
			final Date date) {
		return taskRepository.countByDeliveryBoyAndStatusAndTaskTypeAndUpdatedAtBetween(deliveryBoy, status, taskType, CommonUtility.getDateWithoutTime(date),
				CommonUtility.getTomorrowDateWithoutTime(date));
	}

	@Override
	public Long getTaskCountBasedOnParams(final TaskFilterDTO taskFilterDTO) {
		return taskRepository.getTaskCountBasedOnParams(taskFilterDTO);
	}

	@Override
	public List<Task> getTaskListBasedOnParams(final TaskFilterDTO taskFilterDTO, final Integer startIndex, final Integer pageSize) {
		return taskRepository.getTaskListBasedOnParams(taskFilterDTO, startIndex, pageSize);
	}

	@Override
	public List<Task> getTaskListForOrderId(final Long orderId) {
		return taskRepository.findAllByOrderIdOrderByOrderIdDesc(orderId);
	}

	@Override
	public Task getTaskForOrderIdAndAllocatedFor(final Orders order, final String allocatedFor) throws NotFoundException {
		return taskRepository.findByOrderAndTaskTypeIgnoreCase(order, allocatedFor)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("task.not.found", new Object[] { "Order Id :" + order.getId() })));
	}

	@Override
	public void sendEmailForOrderDeliveryConfirmation(final Long orderId) throws NotFoundException {
		Orders order = orderService.getOrderById(orderId);

		// TODO
		/**
		 * Uncomment the below code during notification implementation
		 */
		// final Notification notification = new Notification();
		// notification.setOrderId(orderId);
		// notification.setEmail(order.getCustomer().getEmail());
		// notification.setCustomerId(order.getCustomer().getId());
		// notification.setType(NotificationQueueConstants.ORDER_DELIVERY_CONFIRMATION);
		// jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
	}

	@Override
	public void sendSmsWhenDeliveryBoyAttemptDelivery(final Long orderId, final Long taskId) throws NotFoundException {
		Orders order = orderService.getOrderById(orderId);
		// TODO
		/**
		 * Uncomment the below code during notification implementation
		 */
		// final Notification notification = new Notification();
		// notification.setOrderId(orderId);
		// notification.setTaskId(taskId);
		// notification.setCustomerId(order.getCustomer().getId());
		// notification.setType(NotificationQueueConstants.ATTEMPTED_ORDER);
		// jmsQueuerService.sendSms(NotificationQueueConstants.SMS_QUEUE, notification);
	}

	@Override
	public TaskResponseDto getTaskDetails(final Long taskId) throws NotFoundException {

		Optional<Task> optTask = taskRepository.findById(taskId);

		if (!optTask.isPresent()) {
			throw new NotFoundException(messageByLocaleService.getMessage("task.not.found", null));
		}
		return convertToResponseDto(optTask.get());

	}

	/**
	 * @param optTask
	 * @return
	 * @throws NotFoundException
	 */
	private TaskResponseDto convertToResponseDto(final Task task) throws NotFoundException {
		TaskResponseDto taskResponseDto = taskMapper.toResponseDto(task);
		Orders order = orderService.getOrderById(task.getOrder().getId());
		VendorResponseDTO vendorResponseDto = vendorService.getVendor(order.getVendor().getId());
		taskResponseDto.setPickupLatitude(vendorResponseDto.getLatitude());
		taskResponseDto.setPickupLongitude(vendorResponseDto.getLongitude());
		taskResponseDto.setVendorAddress(vendorResponseDto.getVendorAddress());
		taskResponseDto.setVendorContactNumber(vendorResponseDto.getContactNo());
		return taskResponseDto;
	}

	@Override
	public void updatePaymentDetailsInTask(final List<Long> taskIds, final Long paymentId) throws ValidationException {
		// TODO : call the paymentdetailsservice and get the paymentdetails object for the id and set it in tasks.
		PaymentDetails paymentDetails = null;

		List<Task> taskList = taskRepository.findAllById(taskIds);
		for (Task task : taskList) {
			if (task.getPaymentDetails() != null) {
				throw new ValidationException(messageByLocaleService.getMessage("payment.already.done.for.task", new Object[] { task.getId() }));
			}
			task.setPaymentDetails(paymentDetails);
			taskRepository.save(task);
		}
	}

	@Override
	public List<TaskResponseDto> getTaskListFromPayment(final Long paymentId) throws ValidationException, NotFoundException {
		// TODO : call the paymentdetailsservice and get the paymentdetails object for the id and set it in tasks.
		PaymentDetails paymentDetails = null;

		List<Task> taskList = taskRepository.findAllByPaymentDetails(paymentDetails);
		List<TaskResponseDto> taskResponseDtoList = new ArrayList<>();
		for (Task task : taskList) {
			TaskResponseDto taskResponseDto = convertToResponseDto(task);
			taskResponseDtoList.add(taskResponseDto);
		}
		return taskResponseDtoList;

	}

	@Override
	public Task getTaskDetail(Long taskId) throws NotFoundException {
		return taskRepository.findById(taskId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("task.not.found", new Object[] {  taskId })));
	}
}
