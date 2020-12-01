/**
 *
 */
package com.nice.controller;

import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Produces;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.TaskStatusEnum;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.TaskResponseDto;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TaskMapper;
import com.nice.model.Task;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.TaskService;
import com.nice.util.PaginationUtil;
import com.nice.validator.TaskValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@RestController
@RequestMapping("/order")
public class TaskController {

	/**
	 *
	 */
	private static final String TASK_UPDATE_MESSAGE = "task.update.message";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TaskValidator taskValidator;

	@Autowired
	private TaskMapper taskMapper;

	@Autowired
	private OrdersService ordersService;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(taskValidator);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TaskController.class);

	/**
	 * complete task:(Used for deliver order)
	 *
	 * @param token
	 * @param userId
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/deliver/{taskId}")
	public ResponseEntity<Object> completeTask(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside complete task method for task Id: {}", taskId);
		Task task = taskService.completeTask(taskId);
		/**
		 * send email start here
		 */
		/**
		 * send email ends here
		 */

		/**
		 * send push notification to vendor about order delivery
		 */
		taskService.sendOrderDeliveryPushNotification(NotificationQueueConstants.ORDER_DELIVERY_PUSH_NOTIFICATION, task);
		/**
		 * send push notification to customer about order delivery
		 */
		taskService.sendOrderDeliveryPushNotification(NotificationQueueConstants.DELIVER_ORDER_PUSH_NOTIFICATION_CUSTOMER, task);

		/**
		 * Send email notification for delivery of order.
		 */
		ordersService.sendEmailNotificationForOrder(NotificationQueueConstants.DELIVER_ORDER_EMAIL_NOTIFICATION_CUSTOMER, task.getOrder().getId());
		/**
		 * to notify customer about order delivery
		 */
		taskService.sendDeliveryInfoToCustomerUsingSocket(task);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(deliveryBoyService.getDashBoard()).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to pickup on way
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/pickup/{taskId}")
	public ResponseEntity<Object> updateStatusToPickOnWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside update task status to pick up on way method for task Id: {}", taskId);
		taskService.updateStatusToPickOnWay(taskId, TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to reached at vendor
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/reach/restaurant/{taskId}")
	public ResponseEntity<Object> updateStatusToReachAtRestaurant(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to reach at restaurant method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.REACHED_VENDOR.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * Update task status to reached at customer
	 *
	 * @param token
	 * @param taskId
	 * @param taskType
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/reach/customer/{taskId}")
	public ResponseEntity<Object> updateStatusToReachToCustomer(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to reach at customer method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.REACHED_CUSTOMER.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to on the way
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/ontheway/{taskId}")
	public ResponseEntity<Object> updateStatusToOneTheWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to one the way method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to return on the way
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/return/ontheway/{taskId}")
	public ResponseEntity<Object> updateStatusToReturnOnTheWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to return on the way method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.RETURN_ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to Replace Customer Pickup On The Way
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/replace/customer/pickup/{taskId}")
	public ResponseEntity<Object> updateStatusToReplaceCustomerPickup(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to repalce customer pickup method for task Id: {}", taskId);
		taskService.updateStatusToPickOnWay(taskId, TaskStatusEnum.REPLACE_CUSTOMER_PICKUP_ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to Replace Customer Pickup On The Way
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/replace/delivery/ontheway/{taskId}")
	public ResponseEntity<Object> updateStatusToReplaceDeliveryOnTheWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to repalce delivery on the way method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.REPLACE_DELIVERY_ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null))
				.setData(/* deliveryBoyService.getDashBoard() */deliveryBoyService.getOrderDetails(taskId, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * @param token
	 * @param paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/payment/{paymentDetailsId}")
	public ResponseEntity<Object> getTaskListForPaymentDetails(@RequestHeader("Authorization") final String token, @PathVariable final Long paymentDetailsId,
			@RequestParam(required = true) final String userType) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get task details method for paymentDetailsId: {}", paymentDetailsId);
		List<TaskResponseDto> taskResponseDtoList = taskService.getTaskListFromPayment(paymentDetailsId, userType);
		LOGGER.info("After get task details method for paymentDetailsId: {}", paymentDetailsId);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("task.list.display.message", null)).setStatus(HttpStatus.OK)
				.setData(taskResponseDtoList).create();
	}

	/**
	 * Get task list for pay-out based on parameters
	 *
	 * @param token
	 * @param pageNumber
	 * @param pageSize
	 * @param taskFilterDTO
	 * @return
	 * @throws ValidationException
	 */
	@PostMapping("/payout/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getTaskListForPayoutBasedOnParams(@RequestHeader("Authorization") final String token, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestBody final TaskFilterDTO taskFilterDTO) throws ValidationException {
		/**
		 * Get Only those tasks which is completed..not in process
		 */
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));

		/**
		 * If the payout is for vendor then no point showing the tasks with order status returncancelled and replacecancelled,
		 * as there wont be any deductions from vendor for such orders and the task status would be cancelled for them
		 */
		if (taskFilterDTO.getVendorId() != null) {
			taskFilterDTO
					.setOrderStatusNotIn(Arrays.asList(OrderStatusEnum.RETURN_CANCELLED.getStatusValue(), OrderStatusEnum.REPLACE_CANCELLED.getStatusValue()));
		}

		Long totalCount = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<Task> resulttasks = taskService.getTaskListBasedOnParams(taskFilterDTO, paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("task.list.display.message", null))
				.setData(taskMapper.toPayoutResponseDtos(resulttasks)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();
	}

	/**
	 * export task list for payout history
	 *
	 * @param accessToken
	 * @param userId
	 * @param httpServletResponse
	 * @param activeRecords
	 * @return
	 * @throws FileOperationException
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Produces("text/csv")
	@PostMapping("/export/payout")
	public ResponseEntity<Object> exportTaskListForPayout(@RequestHeader("Authorization") final String accessToken,
			final HttpServletResponse httpServletResponse, @RequestBody final TaskFilterDTO taskFilterDTO) throws FileOperationException {
		/**
		 * Get Only those tasks which is completed..not in process
		 */
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));

		/**
		 * If the payout is for vendor then no point showing the tasks with order status returncancelled and replacecancelled,
		 * as there wont be any deductions from vendor for such orders.
		 */
		if (taskFilterDTO.getVendorId() != null) {
			taskFilterDTO
					.setOrderStatusNotIn(Arrays.asList(OrderStatusEnum.RETURN_CANCELLED.getStatusValue(), OrderStatusEnum.REPLACE_CANCELLED.getStatusValue()));
		}

		taskService.exportTaskListForPayout(httpServletResponse, taskFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("task.list.display.message", null))
				.create();
	}
}
