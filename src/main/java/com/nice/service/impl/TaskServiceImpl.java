/**
 *
 */
package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.DeliveryType;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.PaymentMode;
import com.nice.constant.SettingsConstant;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.DeliveryBoyOrderCountDto;
import com.nice.dto.DeliveryLogDTO;
import com.nice.dto.DeliveryLogFilterDTO;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.TaskPayoutDTO;
import com.nice.dto.TaskResponseDto;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TaskMapper;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;
import com.nice.model.DeliveryBoyLocation;
import com.nice.model.Orders;
import com.nice.model.PaymentDetails;
import com.nice.model.Task;
import com.nice.model.UserLogin;
import com.nice.repository.DeliveryBoyCurrentStatusRepository;
import com.nice.repository.DeliveryBoyLocationRepository;
import com.nice.repository.OrdersRepository;
import com.nice.repository.TaskRepository;
import com.nice.service.CashcollectionService;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.PaymentDetailsService;
import com.nice.service.SettingsService;
import com.nice.service.TaskService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 16-Jul-2020
 */
@Service(value = "taskService")
@Transactional(rollbackFor = Throwable.class)
public class TaskServiceImpl implements TaskService {
	private static final String INVALID_TASK_STATUS = "invalid.task.status";

	private static final Logger LOGGER = LoggerFactory.getLogger(TaskServiceImpl.class);

	/**
	 *
	 */
	private static final String TASK_NOT_FOUND = "task.not.found";

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
	private CashcollectionService cashCollectionService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private DeliveryBoyLocationService deliveryBoyLocationService;
	@Autowired
	private DeliveryBoyLocationRepository deliveryBoyLocationRepository;

	@Autowired
	private OrdersRepository ordersRepository;

	@Autowired
	private DeliveryBoyCurrentStatusRepository deliveryBoyCurrentStatusRepository;

	@Autowired
	private PaymentDetailsService paymentDetailService;

	@Autowired
	private SettingsService settingsService;

	@Autowired
	private ExportCSV exportCSV;

	@Override
	public Task createTask(final TaskDto taskDto) throws NotFoundException, ValidationException {

		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			taskDto.setDeliveryBoyId(userLogin.getEntityId());
		}

		Orders orders = orderService.getOrderById(taskDto.getOrderId());
		/**
		 * Valdiation to check if the order type is not pick-up the delivery boy should
		 * be assigned to it
		 */
		if (!DeliveryType.PICKUP.getStatusValue().equalsIgnoreCase(orders.getDeliveryType()) && taskDto.getDeliveryBoyId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("specify.delivery.boy.for.order", null));
		}

		/**
		 * Calculate the admin comission here and also the net amount payable to vendor
		 * for the task, this code is only for
		 * regular order, not for replacement or return, for replacement and return the
		 * calculation for the same will be
		 * different.
		 */
		Double adminCommisionRate = (Double) SettingsConstant.getSettingsValue(Constant.ADMIN_COMISSION);
		/**
		 * Here order total amount would be the combination of wallet contribution and
		 * amount paid by the customer
		 */
		Double orderTotal = Double.sum(orders.getTotalOrderAmount(), orders.getWalletContribution());
		Double deliveryCharge = orders.getDeliveryCharge();
		Double adminCommissionAmt = 0.0d;
		Double vendorPayableAmt = 0.0d;
		if (TaskTypeEnum.DELIVERY.getTaskValue().equals(taskDto.getTaskType())) {
			adminCommissionAmt = (orderTotal - deliveryCharge) * adminCommisionRate / 100;
			vendorPayableAmt = orderTotal - deliveryCharge - adminCommissionAmt;
		}
		/**
		 * For return and replacement orders, set the values accordingly, here it is
		 * assumed that the orderTotal will be a +ve
		 * value for return order as well in orders table , keeping that in mind the
		 * below lines have been coded For replacement
		 * orders, there would only be delivery charge and that would be handled in
		 * change status method below, the vendor
		 * payable amount and admin commission would be zero
		 */
		else if (TaskTypeEnum.RETURN.getTaskValue().equals(taskDto.getTaskType())) {
			adminCommissionAmt = (orderTotal - deliveryCharge) * adminCommisionRate / 100;
			vendorPayableAmt = (orderTotal - deliveryCharge - adminCommissionAmt) * -1;
			adminCommissionAmt = adminCommissionAmt * -1;
		} else if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(taskDto.getTaskType())) {
			vendorPayableAmt = vendorPayableAmt - deliveryCharge;
		}

		/**
		 * This code is synchronized as multiple delivery boys trying to accept the same
		 * order for delivery donot end up have
		 * the same order.
		 */
		synchronized (this) {
			Task task = taskMapper.toEntity(taskDto);
			/**
			 * This is because, the task can be created without delivery boy for pickup
			 * orders
			 */
			if (taskDto.getDeliveryBoyId() != null) {
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
				} else if (taskDto.getDeliveryBoyId() != null) {
					DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(taskDto.getDeliveryBoyId());
					orders.setDeliveryBoy(deliveryBoy);
					ordersRepository.save(orders);
					task.setDeliveryBoy(deliveryBoy);
				}
			}
			/**
			 * Task Details
			 */

			task.setOrder(orders);
			task.setStatus(TaskStatusEnum.ORDER_ACCEPTED.getStatusValue());
			task.setTaskType(taskDto.getTaskType());
			task.setActive(true);
			task.setVendorPayableAmt(vendorPayableAmt);
			task.setAdminCommission(adminCommissionAmt);
			task.setTotalOrderAmount(orderTotal);
			task.setVendor(orders.getVendor());
			/**
			 * Actual delivery charge will set at the time of completion of task (see:
			 * change task status method)
			 */
			task.setDeliveryCharge(0d);
			task.setOrderDeliveryType(orders.getDeliveryType());
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
			throw new NotFoundException(messageByLocaleService.getMessage(TASK_NOT_FOUND, new Object[] { taskId }));
		}
		Task task = optTask.get();

		/**
		 * Check if the next task status is allowed
		 */
		TaskStatusEnum existingTaskStatus = TaskStatusEnum.getByValue(task.getStatus());
		if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType()) && !existingTaskStatus.contains(taskStatus)
				|| TaskTypeEnum.RETURN.getTaskValue().equals(task.getTaskType()) && !existingTaskStatus.returnContains(taskStatus)
				|| TaskTypeEnum.REPLACEMENT.getTaskValue().equals(task.getTaskType()) && !existingTaskStatus.replaceContains(taskStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed", new Object[] { taskStatus, task.getStatus() }));
		}

		if (taskStatus.equals(TaskStatusEnum.DELIVERED.getStatusValue())) {
			task.setStatus(TaskStatusEnum.DELIVERED.getStatusValue());
			task.setDeliveredDate(new Date(System.currentTimeMillis()));
			/**
			 * Change the status of order based on the task type, if the task type is
			 * replacement, the order is being replaced and
			 * hence the order should be moved to replaced status, else its first time
			 * delivery and order will be moved to delivered
			 * status, this would be applicable only if there is replacement in place.
			 */
			String nextOrderStatus;
			if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType())) {
				nextOrderStatus = Constant.DELIVERED;
			} else if (TaskTypeEnum.RETURN.getTaskValue().equals(task.getTaskType())) {
				nextOrderStatus = Constant.RETURNED;
			} else if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(task.getTaskType())) {
				nextOrderStatus = Constant.REPLACED;
			} else {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_TASK_STATUS, null));
			}
			orderService.changeStatus(nextOrderStatus, task.getOrder());

			/**
			 * If its a COD task then check cash collected or not
			 */
			if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType()) && PaymentMode.COD.name().equals(task.getOrder().getPaymentMode())
					&& !cashCollectionService.getCashCollectionDetailForTask(task.getId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("cash.collection.not.found.order", new Object[] { task.getOrder().getId() }));
			}
		} else if (taskStatus.equals(TaskStatusEnum.ON_THE_WAY.getStatusValue())) {
			String nextOrderStatus;
			if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType())) {
				nextOrderStatus = Constant.ORDER_PICKED_UP;
			} else {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_TASK_STATUS, null));
			}
			task.setStatus(TaskStatusEnum.ON_THE_WAY.getStatusValue());
			/**
			 * Change order status here to Order PickUp.
			 */
			orderService.changeStatus(nextOrderStatus, task.getOrder());
		} else if (taskStatus.equals(TaskStatusEnum.RETURN_ON_THE_WAY.getStatusValue())) {
			task.setStatus(TaskStatusEnum.RETURN_ON_THE_WAY.getStatusValue());
			/**
			 * Change order status here to Order PickUp.
			 */
			orderService.changeStatus(Constant.RETURN_ORDER_PICKUP, task.getOrder());
		} else if (taskStatus.equals(TaskStatusEnum.REPLACE_DELIVERY_ON_THE_WAY.getStatusValue())) {
			if (OrderStatusEnum.REPLACE_ORDER_PREPARED.getStatusValue().equals(task.getOrder().getOrderStatus())) {
				task.setStatus(TaskStatusEnum.REPLACE_DELIVERY_ON_THE_WAY.getStatusValue());
				/**
				 * Change order status here to Order PickUp.
				 */
				orderService.changeStatus(Constant.REPLACE_ORDER_PICKUP, task.getOrder());
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("waiting.order.prepare", null));
			}

		} else if (taskStatus.equals(TaskStatusEnum.REACHED_VENDOR.getStatusValue()) || taskStatus.equals(TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue())
				|| taskStatus.equals(TaskStatusEnum.REACHED_CUSTOMER.getStatusValue())
				|| taskStatus.equals(TaskStatusEnum.REPLACE_CUSTOMER_PICKUP_ON_THE_WAY.getStatusValue())) {
			task.setStatus(taskStatus);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_TASK_STATUS, null));
		}

		/**
		 * code for delivery boy delivery charge
		 */
		if (taskStatus.equals(TaskStatusEnum.DELIVERED.getStatusValue()) || taskStatus.equals(TaskStatusEnum.CANCELLED.getStatusValue())) {
			String minOrderDelivered = settingsService.getSettingsDetailsByFieldName(Constant.DAY_MIN_ORDER_DELIVERED).getFieldValue();
			/**
			 * if count of today's total delivered or cancelled task for this delivery boy
			 * is greater than minimum order delivered
			 * for a day then consider that tasks
			 */
			TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
			if (task.getDeliveryBoy() != null) {
				taskFilterDTO.setDeliveryBoyId(task.getDeliveryBoy().getId());
			}

			taskFilterDTO.setCreatedAt(new Date(System.currentTimeMillis()));
			taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));

			List<Task> taskList = getTaskListBasedOnParams(taskFilterDTO, null, null);

			/**
			 * Delivery charge for delivery boy
			 */
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(taskList)) {

				if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType())) {
					task.setDeliveryCharge(Double.valueOf(settingsService.getSettingsDetailsByFieldName(Constant.COMMISION_PER_ORDER).getFieldValue()));
				} else if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(task.getTaskType())) {
					task.setDeliveryCharge(Double.valueOf(settingsService.getSettingsDetailsByFieldName(Constant.COMMISION_PER_REPLACE_ORDER).getFieldValue()));
				} else {
					task.setDeliveryCharge(Double.valueOf(settingsService.getSettingsDetailsByFieldName(Constant.COMMISION_PER_RETURN_ORDER).getFieldValue()));
				}
			}

			if (taskList.size() >= Integer.valueOf(minOrderDelivered)) {
				Double incentiveAmount = Double.valueOf(settingsService.getSettingsDetailsByFieldName(Constant.INCENTIVE_AMOUNT_FOR_DAY).getFieldValue());
				task.setDeliveryCharge(Double.sum(task.getDeliveryCharge(), incentiveAmount));
			}
			if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(task.getTaskType()) || TaskTypeEnum.RETURN.getTaskValue().equals(task.getTaskType())) {
				task.setVendorPayableAmt(Double.sum(task.getVendorPayableAmt(), task.getDeliveryCharge() * -1));
			}
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
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("task.not.found.for.order", new Object[] { order.getId() })));
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
		// jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE,
		// notification);
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
			throw new NotFoundException(messageByLocaleService.getMessage(TASK_NOT_FOUND, new Object[] { taskId }));
		}
		return convertToResponseDto(optTask.get());

	}

	/**
	 * @param  optTask
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
		taskResponseDto.setVendorPhoneNumber(vendorResponseDto.getPhoneNumber());
		taskResponseDto.setOrderDate(order.getCreatedAt());
		/**
		 * Get Details related to delivery boy Payment, if payment is done
		 */
		if (task.getDeliveryBoyPaymentDetails() != null) {
			taskResponseDto.setDeliveryBoyPaymentDetailsId(task.getDeliveryBoyPaymentDetails().getId());
			taskResponseDto.setDeliveryBoyTransactionId(task.getDeliveryBoyPaymentDetails().getTransactionNo());
			taskResponseDto.setDeliveryBoyPaidOn(task.getDeliveryBoyPaymentDetails().getCreatedAt());
		}
		/**
		 * Set details related to vendor payment, if payment done
		 */
		if (task.getVendorPaymentDetails() != null) {
			taskResponseDto.setVendorPaymentDetailsId(task.getVendorPaymentDetails().getId());
			taskResponseDto.setVendorTransactionId(task.getVendorPaymentDetails().getTransactionNo());
			taskResponseDto.setVendorPaidOn(task.getVendorPaymentDetails().getCreatedAt());
		}
		return taskResponseDto;
	}

	@Override
	public void updateDeliveryBoyPaymentDetailsInTask(final List<Long> taskIds, final Long paymentId) throws ValidationException, NotFoundException {

		PaymentDetails paymentDetails = paymentDetailService.getPaymentDetailsDetail(paymentId);
		List<Task> taskList = taskRepository.findAllById(taskIds);
		for (Task task : taskList) {
			if (task.getDeliveryBoyPaymentDetails() != null) {
				throw new ValidationException(messageByLocaleService.getMessage("payment.already.done.for.task", new Object[] { task.getId() }));
			}
			task.setDeliveryBoyPaymentDetails(paymentDetails);
			taskRepository.save(task);
		}
	}

	@Override
	public List<TaskResponseDto> getTaskListFromPayment(final Long paymentId, final String userType) throws ValidationException, NotFoundException {

		PaymentDetails paymentDetails = paymentDetailService.getPaymentDetailsDetail(paymentId);
		List<Task> taskList = null;
		if (UserType.VENDOR.name().equals(userType)) {
			taskList = taskRepository.findAllByVendorPaymentDetails(paymentDetails);
		} else if (UserType.DELIVERY_BOY.name().equals(userType)) {
			taskList = taskRepository.findAllByDeliveryBoyPaymentDetails(paymentDetails);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("", null));
		}

		List<TaskResponseDto> taskResponseDtoList = new ArrayList<>();
		for (Task task : taskList) {
			TaskResponseDto taskResponseDto = convertToResponseDto(task);
			taskResponseDtoList.add(taskResponseDto);
		}
		return taskResponseDtoList;

	}

	@Override
	public List<Task> getTaskListForPayout(final TaskFilterDTO taskFilterDTO, final Integer startIndex, final Integer pageSize) {
		return taskRepository.getTaskListBasedOnParams(taskFilterDTO, startIndex, pageSize);
	}

	@Override
	public Task getTaskDetail(final Long taskId) throws NotFoundException {
		return taskRepository.findById(taskId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(TASK_NOT_FOUND, new Object[] { taskId })));
	}

	@Override
	public void updateStatusToPickOnWay(final Long taskId) throws NotFoundException, ValidationException {
		Task task = getTaskDetail(taskId);
		/**
		 * if delivery boy has on going order which is not delivered yet then can not
		 * accept new one
		 */
		TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
		taskFilterDTO.setDeliveryBoyId(task.getDeliveryBoy().getId());
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue(), TaskStatusEnum.REACHED_VENDOR.getStatusValue(),
				TaskStatusEnum.ON_THE_WAY.getStatusValue(), TaskStatusEnum.REACHED_CUSTOMER.getStatusValue(),
				TaskStatusEnum.RETURN_ON_THE_WAY.getStatusValue()));
		Long count = getTaskCountBasedOnParams(taskFilterDTO);
		if (count > 0) {
			throw new ValidationException(messageByLocaleService.getMessage("deliver.order.first", null));
		}

		if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType()) || TaskTypeEnum.RETURN.getTaskValue().equals(task.getTaskType())) {
			changeTaskStatus(taskId, TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue());
		}
	}

	@Override
	public void completeTask(final Long taskId) throws NotFoundException, ValidationException {

		Task task = getTaskDetail(taskId);

		if (TaskTypeEnum.DELIVERY.getTaskValue().equalsIgnoreCase(task.getTaskType())
				&& !OrderStatusEnum.ORDER_PICKED_UP.getStatusValue().equalsIgnoreCase(task.getOrder().getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.for.delivery", null));
		}
		/**
		 * If Order's delivery type is Delivery then order's status should be return
		 * order pickup
		 * If Order's delivery type is pick-up then order's status should be return
		 * processed
		 */
		if (TaskTypeEnum.RETURN.getTaskValue().equalsIgnoreCase(task.getTaskType())
				&& (DeliveryType.DELIVERY.getStatusValue().equals(task.getOrder().getDeliveryType())
						&& !OrderStatusEnum.RETURN_ORDER_PICKUP.getStatusValue().equalsIgnoreCase(task.getOrder().getOrderStatus())
						|| DeliveryType.PICKUP.getStatusValue().equals(task.getOrder().getDeliveryType())
								&& !OrderStatusEnum.RETURN_PROCESSED.getStatusValue().equalsIgnoreCase(task.getOrder().getOrderStatus()))) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.for.delivery", null));
		}
		changeTaskStatus(task.getId(), TaskStatusEnum.DELIVERED.getStatusValue());
		/**
		 * set isBusy to false if delivery boy has no any other assigned orders, no
		 * changes are to be made to delivery boy in
		 * case of pickup orders
		 */
		if (task.getDeliveryBoy() != null) {
			TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
			taskFilterDTO.setDeliveryBoyId(task.getDeliveryBoy().getId());
			taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
			Long count = getTaskCountBasedOnParams(taskFilterDTO);
			/**
			 * if count > 0 means delivery boy has any orders which is not delivered yet
			 */
			if (count == 0) {
				DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = deliveryBoyService.getDeliveryBoyCurrentStatusDetail(task.getDeliveryBoy());
				deliveryBoyCurrentStatus.setIsBusy(false);
				deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
				/**
				 * remove delivery boy's old location history accepts his latest location
				 */
				List<DeliveryBoyLocation> oldLocations = deliveryBoyLocationService.getDeliveryBoyLocationList(task.getDeliveryBoy().getId(), false);
				if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(oldLocations)) {
					LOGGER.info("Deleting old Delivery boy's locations for delivery boy:{}", task.getDeliveryBoy().getId());
					deliveryBoyLocationRepository.deleteAll(oldLocations);
				}
			}
		}
	}

	@Override
	public DeliveryBoyOrderCountDto getTaskTypeWiseCountForPaymentDetailsId(final Long deliveryBoyId) {
		DeliveryBoyOrderCountDto deliveryBoyOrderCount = new DeliveryBoyOrderCountDto();
		deliveryBoyOrderCount.setCartOrders(taskRepository.getCountCartOrderCountForDeliveryPerson(deliveryBoyId));
		deliveryBoyOrderCount.setReplaceOrders(taskRepository.getCountReplacementOrderCountForDeliveryPerson(deliveryBoyId));
		deliveryBoyOrderCount.setReturnOrders(taskRepository.getCountReturnOrderCountForDeliveryPerson(deliveryBoyId));
		deliveryBoyOrderCount.setTotalAmountPaid(taskRepository.getTotalDeliveryChargeForDeliveryPerson(deliveryBoyId));
		return deliveryBoyOrderCount;
	}

	@Override
	public List<DeliveryLogDTO> getTaskListForDeliveryLog(final DeliveryLogFilterDTO deliveryLogFilterDTO, final Integer startIndex, final Integer pageSize) {
		List<Task> taskList = taskRepository.getTaskListForDeliveryLogBasedOnParams(deliveryLogFilterDTO, startIndex, pageSize);
		return convertToDeliveryLog(taskList);
	}

	private List<DeliveryLogDTO> convertToDeliveryLog(final List<Task> tasks) {
		List<DeliveryLogDTO> deliveryLogDTOs = new ArrayList<>();
		for (Task task : tasks) {
			DeliveryLogDTO deliveryLogDTO = new DeliveryLogDTO();
			deliveryLogDTO.setAssignedDate(task.getUpdatedAt());
			Customer customer = task.getOrder().getCustomer();
			deliveryLogDTO.setCustomerId(customer.getId());
			deliveryLogDTO.setCustomerName(customer.getFirstName().concat(" ").concat(customer.getLastName()));
			deliveryLogDTO.setCustomerEmail(customer.getEmail());
			if (task.getDeliveryBoy() != null) {
				deliveryLogDTO.setDeliveryBoyId(task.getDeliveryBoy().getId());
				deliveryLogDTO.setDeliveryBoyEmail(task.getDeliveryBoy().getEmail());
			}
			deliveryLogDTO.setOrderDate(task.getOrder().getCreatedAt());
			deliveryLogDTO.setOrderId(task.getOrder().getId());
			deliveryLogDTO.setTaskStatus(task.getStatus());
			deliveryLogDTO.setVendorId(task.getVendor().getId());
			deliveryLogDTO.setTaskType(task.getTaskType());
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				if (task.getDeliveryBoy() != null) {
					deliveryLogDTO
							.setDeliveryBoyName(task.getDeliveryBoy().getFirstNameEnglish().concat(" ").concat(task.getDeliveryBoy().getLastNameEnglish()));
				}
				deliveryLogDTO.setVendorName(task.getVendor().getFirstNameEnglish().concat(" ").concat(task.getVendor().getLastNameEnglish()));
				deliveryLogDTO.setVendorStoreName(task.getVendor().getStoreNameEnglish());
			} else {
				if (task.getDeliveryBoy() != null) {
					deliveryLogDTO.setDeliveryBoyName(task.getDeliveryBoy().getFirstNameArabic().concat(" ").concat(task.getDeliveryBoy().getLastNameArabic()));
				}
				deliveryLogDTO.setVendorName(task.getVendor().getFirstNameArabic().concat(" ").concat(task.getVendor().getLastNameArabic()));
				deliveryLogDTO.setVendorStoreName(task.getVendor().getStoreNameArabic());
			}
			deliveryLogDTOs.add(deliveryLogDTO);
		}
		return deliveryLogDTOs;
	}

	@Override
	public Long getTaskCountForDeliveryLog(final DeliveryLogFilterDTO deliveryLogFilterDTO) {
		return taskRepository.getTaskCountForDeliveryLogBasedOnParams(deliveryLogFilterDTO);
	}

	@Override
	public void exportDeliveryLogList(final DeliveryLogFilterDTO deliveryLogFilterDTO, final HttpServletResponse httpServletResponse)
			throws FileNotFoundException, ValidationException {
		List<DeliveryLogDTO> deliveryLogDTOs = getTaskListForDeliveryLog(deliveryLogFilterDTO, null, null);
		final Object[] deliveryLogHeaderField = new Object[] { "Order Id", "Order Date", "Customer Name", "Customer Email", "Delivery Boy Name",
				"Delivery Boy Email", "Assigned On", "Vendor Name", "Status", "Order type" };
		final Object[] deliveryLogDataField = new Object[] { "orderId", "orderDate", "customerName", "customerEmail", "deliveryBoyName", "deliveryBoyEmail",
				"assignedDate", "vendorStoreName", "taskStatus", "taskType" };
		try {
			exportCSV.writeCSVFile(deliveryLogDTOs, deliveryLogDataField, deliveryLogHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}

	@Override
	public void exportTaskListForPayout(final HttpServletResponse httpServletResponse, final TaskFilterDTO taskFilterDTO) throws FileOperationException {
		final Object[] taskPayoutHeaderField;
		final Object[] taskPayoutDataField;
		List<TaskPayoutDTO> taskPayoutDTOs = taskMapper.toPayoutResponseDtos(getTaskListBasedOnParams(taskFilterDTO, null, null));
		for (TaskPayoutDTO taskPayoutDTO : taskPayoutDTOs) {
			if (TaskTypeEnum.DELIVERY.getTaskValue().equals(taskPayoutDTO.getTaskType())) {
				taskPayoutDTO.setOrderType("Cart Order");
			} else if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(taskPayoutDTO.getTaskType())) {
				taskPayoutDTO.setOrderType("Replacement Order");
			} else {
				taskPayoutDTO.setOrderType("Return Order");
			}
			if (taskFilterDTO.getDeliveryBoyPaymentPending() != null) {
				if (taskPayoutDTO.getDeliveryBoyPaymentDetailsId() != null) {
					taskPayoutDTO.setPaidOn(taskPayoutDTO.getDeliveryBoyPaidOn());
					taskPayoutDTO.setTransactionId(taskPayoutDTO.getDeliveryBoyTransactionId());
				} else {
					taskPayoutDTO.setPaymentStatus("Pending");
				}
			} else {
				if (taskPayoutDTO.getVendorPaymentDetailsId() != null) {
					taskPayoutDTO.setPaidOn(taskPayoutDTO.getVendorPaidOn());
					taskPayoutDTO.setTransactionId(taskPayoutDTO.getVendorTransactionId());
				} else {
					taskPayoutDTO.setPaymentStatus("Pending");
				}
			}
		}
		if (taskFilterDTO.getDeliveryBoyPaymentPending() != null) {
			taskPayoutHeaderField = new Object[] { "Order Id", "Order Date", "Attended On", "Order Type", "Current Order Status", "Delivery Charge",
					"Order Amount", "Payment Status", "Paid On", "Transaction Id" };
			taskPayoutDataField = new Object[] { "orderId", "orderDate", "deliveredDate", "orderType", "orderStatus", "deliveryCharge", "totalOrderAmount",
					"paymentStatus", "paidOn", "transactionId" };
		} else {
			taskPayoutHeaderField = new Object[] { "Order Id", "Order Date", "Order Type", "Total Order Amount", "Delivery Charge", "Admin Commission",
					"Payable Amount", "Payment Status", "Paid On", "Transaction Id" };
			taskPayoutDataField = new Object[] { "orderId", "orderDate", "orderType", "totalOrderAmount", "deliveryCharge", "adminCommission",
					"vendorPayableAmt", "paymentStatus", "paidOn", "transactionId" };
		}
		try {
			exportCSV.writeCSVFile(taskPayoutDTOs, taskPayoutDataField, taskPayoutHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}
}
