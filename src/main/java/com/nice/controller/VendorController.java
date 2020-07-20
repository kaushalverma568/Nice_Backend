package com.nice.controller;

import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
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

import com.nice.constant.Constant;
import com.nice.constant.VendorStatus;
import com.nice.dto.EmailUpdateDTO;
import com.nice.dto.LoginResponse;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.UserLoginDto;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.UnAuthorizationException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorMapper;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.PaginationUtil;
import com.nice.validator.VendorValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */
@RequestMapping(path = "/vendor")
@RestController
public class VendorController {
	/**
	 *
	 */
	private static final String VENDOR_VALIDATION_FAILED = "vendor validation failed";
	/**
	 *
	 */
	private static final String VENDOR_UPDATE_MESSAGE = "vendor.update.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(VendorController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding vendor
	 */
	@Autowired
	private VendorValidator vendorValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(vendorValidator);
	}

	@Autowired
	private VendorService vendorService;

	@Autowired
	private TokenStore tokenStore;

	@Autowired
	private VendorMapper vendorMapper;

	@Autowired
	private UserLoginService userLoginService;

	/**
	 * Add Vendor
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	@PostMapping
	public ResponseEntity<Object> addVendor(@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture,
			@ModelAttribute @Valid final VendorDTO vendorDTO, final BindingResult result) throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Vendor validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		vendorService.addVendor(vendorDTO, profilePicture);
		LOGGER.info("Outside add Vendor ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.create.message", null))
				.create();
	}

	/**
	 * Update vendor's personal details
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	public ResponseEntity<Object> updatePersonalDetails(@RequestHeader("Authorization") final String accessToken,
			@ModelAttribute @Valid final VendorDTO vendorDTO, final BindingResult result,
			@RequestParam(name = "profilePicture", required = false) final MultipartFile profilePicture) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update vendor {}", vendorDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		vendorService.updatePersonalDetails(vendorDTO, profilePicture);
		LOGGER.info("Outside update vendor");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update bank details
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/bank/details")
	public ResponseEntity<Object> updateBankDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final VendorBankDetailsDTO vendorBankDetailsDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update account details {}", vendorBankDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		vendorService.updateBankDetails(vendorBankDetailsDTO);
		LOGGER.info("Outside update account details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update restaurant details
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/restaurant/details")
	public ResponseEntity<Object> updateRestaurantDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update restaurant details {}", vendorRestaurantDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		vendorService.updateRestaurantDetails(vendorRestaurantDetailsDTO);
		LOGGER.info("Outside restaurant account details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update subscription plan
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  vendorBankDetailsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/{vendorId}/{subscriptionPlanId}")
	public ResponseEntity<Object> updateSubscriptionPlan(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @PathVariable("subscriptionPlanId") final Long subscriptionPlanId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside update subscription plan for vendor:{} and planId:{}", vendorId, subscriptionPlanId);
		vendorService.addUpdateSubscriptionPlan(vendorId, subscriptionPlanId);
		LOGGER.info("Outside update subscription plan ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update order service enable or not for vendor
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  vendorBankDetailsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/orderservice/enable/{vendorId}")
	public ResponseEntity<Object> updateOrderServiceEnableForVendor(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @RequestParam("isOrderServiceEnable") final Boolean isOrderServiceEnable)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update order service enable for vendor:{} and isOrderServiceEnable:{}", vendorId, isOrderServiceEnable);
		vendorService.updateOrderServiceEnableForVendor(vendorId, isOrderServiceEnable);
		LOGGER.info("Outside update order service enable for vendor ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get Vendor
	 *
	 * @param  vendorId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{vendorId}")
	public ResponseEntity<Object> getVendor(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId)
			throws NotFoundException {
		LOGGER.info("Inside get Vendor for id:{}", vendorId);
		final VendorResponseDTO resultVendorResponseDTO = vendorService.getVendor(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.detail.message", null))
				.setData(resultVendorResponseDTO).create();
	}

	/**
	 * Get Vendor Bank details
	 *
	 * @param  vendorId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/bank/details/{vendorId}")
	public ResponseEntity<Object> getVendorBankDetails(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId)
			throws NotFoundException {
		LOGGER.info("Inside get Vendor for id:{}", vendorId);
		final VendorBankDetails resultVendorResponseDTO = vendorService.getVendorBankDetails(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.detail.message", null))
				.setData(vendorMapper.toBankDetailsDTO(resultVendorResponseDTO)).create();
	}

	/**
	 * Get vendor list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	@PutMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getVendorListBasedOnParams(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestBody final VendorFilterDTO vendorFilterDTO) throws ValidationException {
		Long totalCount = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		List<Vendor> vendorList = vendorService.getVendorListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, vendorFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.list.message", null))
				.setData(vendorMapper.toDtos(vendorList)).setHasNextPage(paginationUtilDto.getHasNextPage())
				.setHasPreviousPage(paginationUtilDto.getHasPreviousPage()).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).create();

	}

	/**
	 * Change Status of Vendor (Active/DeActive)
	 *
	 * @param  vendorId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/status/{vendorId}")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of Vendor of id {} and status {}", vendorId, active);
		String userName = vendorService.changeStatus(vendorId, active);
		if (userName != null) {
			revokeToken(userName);
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * revoke token for the user
	 *
	 * @param userName
	 * @param userId
	 */
	private void revokeToken(final String userName) {
		LOGGER.info("Revoking token for user {} }", userName);
		Collection<OAuth2AccessToken> tokens = tokenStore.findTokensByClientIdAndUserName(Constant.CLIENT_ID, userName);
		for (OAuth2AccessToken token : tokens) {
			tokenStore.removeAccessToken(token);
		}
		LOGGER.info("Successfully Revoked token for user {}", userName);
	}

	/**
	 * Get vendor list for customer app
	 *
	 * @param  vendorListFilterDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/app/list")
	public ResponseEntity<Object> getVendorCustomerListBasedOnParams(@RequestBody @Valid final VendorListFilterDTO vendorListFilterDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		List<VendorResponseDTO> vendorList = vendorService.getVendorListForApp(vendorListFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.list.message", null))
				.setData(vendorList).create();

	}

	@PutMapping("/change/status/{vendorId}/{newStatus}")
	public ResponseEntity<Object> changeVendorStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@PathVariable("newStatus") final String newStatus) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of Vendor of id {} and status {}", vendorId, newStatus);
		String userName = vendorService.changeVendorStatus(vendorId, newStatus);
		if (userName != null && VendorStatus.SUSPENDED.getStatusValue().equals(newStatus)) {
			revokeToken(userName);
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	@GetMapping("/export/list")
	public ResponseEntity<Object> exportList(@RequestHeader("Authorization") final String accessToken, @RequestHeader("userId") final Long userId,
			final HttpServletResponse httpServletResponse, @RequestParam(name = "activeRecords", required = false) final Boolean activeRecords)
			throws IOException {
		vendorService.exportVendorList(activeRecords, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.list.message", null))
				.create();
	}

	@PutMapping("/change/email/{vendorId}")
	public ResponseEntity<Object> changeVendorEmail(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestBody @Valid final EmailUpdateDTO emailUpdateDTO, final BindingResult result)
			throws NotFoundException, ValidationException, UnAuthorizationException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		LOGGER.info("Inside change email of Vendor of id {} and email {} and otp {}", vendorId, emailUpdateDTO.getEmail(), emailUpdateDTO.getOtp());
		String userName = vendorService.changeVendorEmail(vendorId, emailUpdateDTO);
		LOGGER.info("Outside change email of vendor");
		revokeToken(userName);
		UserLoginDto userLoginDto = new UserLoginDto();
		userLoginDto.setUserName(emailUpdateDTO.getEmail());
		userLoginDto.setPassword(emailUpdateDTO.getPassword());
		LoginResponse loginResponse = userLoginService.adminLogin(userLoginDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.setData(loginResponse).create();
	}

	@PutMapping("/email/generate/{vendorId}")
	public ResponseEntity<Object> generateOTPForChangeEmail(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @RequestParam(name = "email", required = true) final String email)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside generate otp for change email of Vendor of id {} and email {} ", vendorId, email);
		String otp = vendorService.generateOTPForChangeEmail(email, vendorId);
		return new GenericResponseHandlers.Builder().setData(otp).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("otp.generated.success", null)).create();
	}

	@PutMapping("/change/contact/{vendorId}")
	public ResponseEntity<Object> changeVendorContact(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestParam(name = "contactNo", required = true) final String contactNo, @RequestParam(name = "otp", required = true) final String otp)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside change Contact of Vendor of id {} and contact {} and otp {}", vendorId, contactNo, otp);
		vendorService.changeVendorContact(vendorId, contactNo, otp);
		LOGGER.info("Outside change contact of vendor");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	@PutMapping("/contact/generate/{vendorId}")
	public ResponseEntity<Object> generateOTPForChangeContact(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @RequestParam(name = "contactNo", required = true) final String contactNo)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside generate otp for change contact of Vendor of id {} and contact no {} ", vendorId, contactNo);
		String otp = vendorService.generateOTPForChangeContact(contactNo, vendorId);
		return new GenericResponseHandlers.Builder().setData(otp).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("otp.generated.success", null)).create();
	}

	@PutMapping("/contact/verify/generate/{vendorId}")
	public ResponseEntity<Object> generateOTPForVerifyContact(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside generate otp for change contact of Vendor of id {} ", vendorId);
		String otp = vendorService.generateOTPForVerifyContact(vendorId);
		return new GenericResponseHandlers.Builder().setData(otp).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("otp.generated.success", null)).create();
	}

	@PutMapping("/verify/contact/{vendorId}")
	public ResponseEntity<Object> verifyVendorContact(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestParam(name = "otp", required = true) final String otp) throws NotFoundException, ValidationException {
		LOGGER.info("Inside verify Contact of Vendor of id {} and otp {}", vendorId, otp);
		vendorService.verifyVendorContact(vendorId, otp);
		LOGGER.info("Outside verify contact of vendor");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}
}
