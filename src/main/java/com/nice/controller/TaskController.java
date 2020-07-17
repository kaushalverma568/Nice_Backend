/**
 *
 */
package com.nice.controller;

import java.util.List;
import java.util.Optional;

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
import org.springframework.web.bind.annotation.RestController;

import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskResponseDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Task;
import com.nice.repository.TaskRepository;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TaskService;
import com.nice.validator.TaskValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@RestController
@RequestMapping("/task")
public class TaskController {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private TaskRepository taskRepository;

	@Autowired
	private TaskValidator taskValidator;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(taskValidator);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TaskController.class);

	/**
	 *
	 * @param token
	 * @param taskDto
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/accept")
	public ResponseEntity<Object> acceptTask(@RequestHeader("Authorization") final String token, @RequestBody final TaskDto taskDto)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside accept task method for order Id: {}", taskDto.getOrderId());
		Task task = taskService.createTask(taskDto);
		LOGGER.info("Successfully accepted task method for order Id : {} and generated task Id :{}", taskDto.getOrderId(), task.getId());
		return new GenericResponseHandlers.Builder().setData(null).create();
	}

	/**
	 *
	 * @param token
	 * @param userId
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/deliver/{taskId}")
	public ResponseEntity<Object> completeTask(@RequestHeader("Authorization") final String token, @RequestHeader final Long userId,
			@PathVariable final Long taskId) throws ValidationException, NotFoundException {

		taskService.changeTaskStatus(taskId, TaskStatusEnum.DELIVERED.getStatusValue());
		/**
		 * send email start here
		 */
		/**
		 * send email to customer when order delivered successfully
		 */
		Optional<Task> optTask = taskRepository.findById(taskId);
		if (optTask.isPresent() && TaskTypeEnum.DELIVERY.name().equalsIgnoreCase(optTask.get().getTaskType())) {
			taskService.sendEmailForOrderDeliveryConfirmation(optTask.get().getOrder().getId());
		}
		if (optTask.isPresent()) {
			/**
			 * Push Notification code
			 */
			// ordersService.sendPushNotificationOnStatus(optTask.get().getOrder().getId(),
			// NotificationQueueConstants.ORDER_DELIVERY_CONFIRMATION_FOR_DELIVERY_BOY);
		} /**
			 * send email ends here
			 */
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("task.update.message", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/reach/restaurant/{taskId}")
	public ResponseEntity<Object> attemptTask(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {

		taskService.changeTaskStatus(taskId, TaskStatusEnum.REACHED_VENDOR.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("task.update.message", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 *
	 * @param token
	 * @param taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/order/pickup/{taskId}")
	public ResponseEntity<Object> orderPickup(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {

		taskService.changeTaskStatus(taskId, TaskStatusEnum.ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("task.update.message", null)).setStatus(HttpStatus.OK)
				.create();
	}

	@GetMapping("/payment/{paymentDetailsId}")
	public ResponseEntity<Object> getTaskListForPaymentDetails(@RequestHeader("Authorization") final String token, @PathVariable final Long paymentDetailsId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get task details method for paymentDetailsId: {}", paymentDetailsId);
		List<TaskResponseDto> taskResponseDtoList = taskService.getTaskListFromPayment(paymentDetailsId);
		LOGGER.info("After get task details method for paymentDetailsId: {}", paymentDetailsId);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("task.list.display.message", null)).setStatus(HttpStatus.OK)
				.setData(taskResponseDtoList).create();
	}

}
