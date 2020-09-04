/**
 *
 */
package com.nice.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.constant.TaskStatusEnum;
import com.nice.dto.TaskResponseDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TaskService;
import com.nice.validator.TaskValidator;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 15-Jul-2020
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
	private TaskValidator taskValidator;

	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(taskValidator);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(TaskController.class);

	/**
	 * complete task:(Used for deliver order)
	 *
	 * @param  token
	 * @param  userId
	 * @param  taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/deliver/{taskId}")
	public ResponseEntity<Object> completeTask(@RequestHeader("Authorization") final String token, @RequestHeader final Long userId,
			@PathVariable final Long taskId) throws ValidationException, NotFoundException {
		LOGGER.info("Inside complete task method for task Id: {}", taskId);
		taskService.completeTask(taskId);
		/**
		 * send email start here
		 */
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to pickup on way
	 *
	 * @param  token
	 * @param  taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/pickup/{taskId}")
	public ResponseEntity<Object> updateStatusToPickOnWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside update task status to pick up on way method for task Id: {}", taskId);
		taskService.updateStatusToPickOnWay(taskId);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to reached at vendor
	 *
	 * @param  token
	 * @param  taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/reach/restaurant/{taskId}")
	public ResponseEntity<Object> updateStatusToReachAtRestaurant(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to reach at restaurant method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.REACHED_VENDOR.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 * update task status to on the way
	 *
	 * @param  token
	 * @param  taskId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/ontheway/{taskId}")
	public ResponseEntity<Object> updateStatusToOneTheWay(@RequestHeader("Authorization") final String token, @PathVariable final Long taskId)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update task status to one the way method for task Id: {}", taskId);
		taskService.changeTaskStatus(taskId, TaskStatusEnum.ON_THE_WAY.getStatusValue());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage(TASK_UPDATE_MESSAGE, null)).setStatus(HttpStatus.OK).create();
	}

	/**
	 *
	 * @param  token
	 * @param  paymentDetailsId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
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
