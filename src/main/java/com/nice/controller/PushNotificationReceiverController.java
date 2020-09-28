package com.nice.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PushNotificationReceiverMapper;
import com.nice.model.PushNotificationReceiver;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PushNotificationReceiverService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@RequestMapping(path = "/push/notification")
@RestController
public class PushNotificationReceiverController {

	private static final Logger LOGGER = LoggerFactory.getLogger(PushNotificationReceiverController.class);

	@Autowired
	private PushNotificationReceiverService pushNotificationReceiverService;

	@Autowired
	private PushNotificationReceiverMapper pushNotificationReceiverMapper;

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
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize,
			@RequestParam(name = "deviceId", required = true) final String deviceId) throws NotFoundException, ValidationException {
		final Page<PushNotificationReceiver> pushnotificationReceiverList = pushNotificationReceiverService.getPushNotificationListForUser(pageNumber, pageSize,
				deviceId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("notification.list.message", null))
				.setData(pushNotificationReceiverMapper.toDtos(pushnotificationReceiverList.getContent()))
				.setHasNextPage(pushnotificationReceiverList.hasNext()).setHasPreviousPage(pushnotificationReceiverList.hasPrevious())
				.setTotalPages(pushnotificationReceiverList.getTotalPages()).setPageNumber(pushnotificationReceiverList.getNumber() + 1)
				.setTotalCount(pushnotificationReceiverList.getTotalElements()).create();
	}

	/**
	 * Delete push notification receiver by id
	 *
	 * @param  accessToken
	 * @param  countryId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping("/{pushNotificationReceiverId}")
	public ResponseEntity<Object> deletePushNotificationReceiver(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("pushNotificationReceiverId") final Long pushNotificationReceiverId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete push notification receiver id:{}", pushNotificationReceiverId);
		pushNotificationReceiverService.deletePushNotificationReceiver(pushNotificationReceiverId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("notification.delete.message", null))
				.create();
	}

}
