package com.nice.controller;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.ConsumerTokenServices;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyMapper;
import com.nice.model.DeliveryBoy;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DeliveryBoyService;
import com.nice.util.CommonUtility;
import com.nice.validator.DeliveryBoyValidator;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@RequestMapping(path = "/deliveryboy")
@RestController
public class DeliveryBoyController {

	private static final String DELIVERYBOY_UPDATE_MESSAGE = "deliveryboy.update.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DeliveryBoyController.class);
	/**
	 * Locale message service - to display response messages from
	 * messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding deliveryBoy
	 */
	@Autowired
	private DeliveryBoyValidator deliveryBoyValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(deliveryBoyValidator);
	}

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TokenStore tokenStore;

	@Autowired
	private DeliveryBoyMapper deliveryBoyMapper;

	@Autowired
	private ConsumerTokenServices consumerTokenServices;

	/**
	 * Add DeliveryBoy
	 *
	 * @param deliveryBoyDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	@PostMapping
	public ResponseEntity<Object> addDeliveryBoy(@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture,
			@ModelAttribute @Valid final DeliveryBoyDTO deliveryBoyDTO, final BindingResult result) throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("DeliveryBoy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (profilePicture == null || !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(profilePicture.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage("profile.image.required", null));
		}
		deliveryBoyService.addDeliveryBoy(deliveryBoyDTO, profilePicture);
		LOGGER.info("Outside add DeliveryBoy ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.create.message", null))
				.create();
	}

	/**
	 * Update personal details
	 *
	 * @param deliveryBoyDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping()
	public ResponseEntity<Object> updatePersonalDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update personal details {}", deliveryBoyPersonalDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("delivery boy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		String userName = deliveryBoyService.updatePersonalDetails(deliveryBoyPersonalDetailsDTO);
		/**
		 * revoke token at the time of changing an email
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userName)) {
			revokeToken(userName);
		}
		LOGGER.info("Outside update personal details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update account details
	 *
	 * @param deliveryBoyDTO
	 * @param result
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/account/details")
	public ResponseEntity<Object> updateAccountDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final DeliveryBoyAccountDetailsDTO deliveryBoyAccountDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update account details {}", deliveryBoyAccountDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("delivery boy validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		deliveryBoyService.updateAccountDetails(deliveryBoyAccountDetailsDTO);
		LOGGER.info("Outside update account details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get DeliveryBoy
	 *
	 * @param deliveryBoyId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{deliveryBoyId}")
	public ResponseEntity<Object> getDeliveryBoy(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException {
		LOGGER.info("Inside get DeliveryBoy for id:{}", deliveryBoyId);
		final DeliveryBoyResponseDTO resultDeliveryBoyResponseDTO = deliveryBoyService.getDeliveryBoy(deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.detail.message", null))
				.setData(resultDeliveryBoyResponseDTO).create();
	}

	/**
	 * Get DeliveryBoy List
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getDeliveryBoyList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords,
			@RequestParam(name = "searchKeyword", required = false) final String searchKeyword) throws NotFoundException {
		LOGGER.info("Inside get delivery boy List");
		final Page<DeliveryBoy> resultDeliveryBoyPages = deliveryBoyService.getDeliveryBoyList(pageNumber, pageSize, activeRecords, searchKeyword);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("deliveryboy.list.message", null))
				.setData(deliveryBoyMapper.toDtos(resultDeliveryBoyPages.getContent())).setHasNextPage(resultDeliveryBoyPages.hasNext())
				.setHasPreviousPage(resultDeliveryBoyPages.hasPrevious()).setTotalPages(resultDeliveryBoyPages.getTotalPages())
				.setPageNumber(resultDeliveryBoyPages.getNumber() + 1).setTotalCount(resultDeliveryBoyPages.getTotalElements()).create();
	}

	/**
	 * Change Status of DeliveryBoy (Active/DeActive)
	 *
	 * @param deliveryBoyId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{deliveryBoyId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @RequestParam("active") final Boolean active)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of delivery boy of id {} and status {}", deliveryBoyId, active);
		String userName = deliveryBoyService.changeStatus(deliveryBoyId, active);
		if (userName != null) {
			revokeToken(userName);
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * update profile picture of delivery boy
	 *
	 * @param deliveryBoyId
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/profilepicture/{deliveryBoyId}")
	public ResponseEntity<Object> updateProfilePicture(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside update profile picture of delivery boy id:{}", deliveryBoyId);
		if (profilePicture == null || !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(profilePicture.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage("profile.image.required", null));
		}
		deliveryBoyService.updateProfilePicture(profilePicture, deliveryBoyId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("profile.image.update.message", null)).create();
	}

	/**
	 * Accept order
	 *
	 * @param accessToken
	 * @param deliveryBoyId
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/accept/order/{deliveryBoyId}/{orderId}")
	public ResponseEntity<Object> acceptOrder(@RequestHeader("Authorization") final String accessToken, @PathVariable("deliveryBoyId") final Long deliveryBoyId,
			@PathVariable("orderId") final Long orderId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside accept order by delivery boy {} and order {}", deliveryBoyId, orderId);
		deliveryBoyService.acceptOrder(deliveryBoyId, orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Deliver order
	 *
	 * @param orderId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/deliver/order/{deliveryBoyId}/{orderId}")
	public ResponseEntity<Object> deliverOrder(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("deliveryBoyId") final Long deliveryBoyId, @PathVariable("orderId") final Long orderId) throws NotFoundException {
		LOGGER.info("Inside deliver order by delivery boy {} and order {}", deliveryBoyId, orderId);
		deliveryBoyService.deliverOrder(deliveryBoyId, orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(DELIVERYBOY_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * revoke token for the user
	 *
	 * @param userName
	 * @param userId
	 */
	private void revokeToken(final String userName) {
		LOGGER.info("Revoking token for user {} ", userName);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName("grocerus-client", userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {}", userName);
	}

	/**
	 * log out delivery boy
	 *
	 * @param accessToken
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/logout")
	public ResponseEntity<Object> logout(@RequestHeader("Authorization") final String accessToken) throws NotFoundException, ValidationException {
		LOGGER.info("Inside Log out method for delivery boy");
		deliveryBoyService.validateBeforeLogout();
		String tokenValue = accessToken.replace("Bearer", "").trim();
		consumerTokenServices.revokeToken(tokenValue);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("logout.message", null)).create();
	}
}
