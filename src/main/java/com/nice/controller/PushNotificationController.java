package com.nice.controller;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.PaginationUtilDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PushNotificationMapper;
import com.nice.model.PushNotification;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PushNotificationService;
import com.nice.util.PaginationUtil;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@RequestMapping(path = "/push/notification")
@RestController
public class PushNotificationController {

	private static final Logger LOGGER = LoggerFactory.getLogger(PushNotificationController.class);

	@Autowired
	private PushNotificationService pushNotificationService;

	@Autowired
	private PushNotificationMapper pushNotificationMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Get push notification list for user
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/list/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getPushNotificationListForUser(@RequestHeader("Authorization") final String accessToken,
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize) throws NotFoundException, ValidationException {
		Long totalCount = pushNotificationService.getCountOfPushNotificationForUser();
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<PushNotification> pushnotificationReceiverList = pushNotificationService.getPushNotificationListForUser(paginationUtilDto.getStartIndex(),
				pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("notification.list.message", null))
				.setData(pushNotificationMapper.toDtos(pushnotificationReceiverList)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();
	}

	/**
	 * Get today's count push notification
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/count")
	public ResponseEntity<Object> getTodaysPushNotificationCountForUser(@RequestHeader("Authorization") final String accessToken)
			throws NotFoundException, ValidationException {
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("notification.list.message", null))
				.setData(pushNotificationService.getTodaysPushNotificationCountForUser()).create();
	}

	/**
	 * Delete push notification by id
	 *
	 * @param  accessToken
	 * @param  countryId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping("/{pushNotificationId}")
	public ResponseEntity<Object> deletePushNotificationReceiver(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("pushNotificationId") final Long pushNotificationId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete push notification by id:{}", pushNotificationId);
		pushNotificationService.deletePushNotification(pushNotificationId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("notification.delete.message", null))
				.create();
	}

}
