package com.nice.controller;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import com.amazonaws.services.pinpoint.model.GPSCoordinates;
import com.nice.dto.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.oauth2.common.OAuth2AccessToken;
import org.springframework.security.oauth2.provider.token.TokenStore;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.DeleteMapping;
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
import org.springframework.web.servlet.ModelAndView;

import com.google.gson.Gson;
import com.nice.constant.Constant;
import com.nice.constant.SuccessErrorType;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorMapper;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.HesabePaymentService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorPaymentService;
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
	private static final String VENDOR_DETAIL_MESSAGE = "vendor.detail.message";
	/**
	 *
	 */
	private static final String VENDOR_LIST_MESSAGE = "vendor.list.message";
	/**
	 *
	 */
	private static final String VENDOR_VALIDATION_FAILED = "vendor validation failed";
	/**
	 *
	 */
	private static final String VENDOR_UPDATE_MESSAGE = "vendor.update.message";

	private static final String REDIRECT = "redirect:";

	@Value("${admin.url}")
	private String adminUrl;
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

	@Autowired
	private HesabePaymentService hesabePaymentService;

	@Autowired
	private VendorPaymentService vendorPaymentService;


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
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	@PostMapping
	public ResponseEntity<Object> addVendor(@RequestParam("id") Long id, @RequestParam("businessCategoryId") Long businessCategoryId, @RequestParam("email") String email, @RequestParam("password") String password,
											@RequestParam("firstNameArabic") String firstNameArabic, @RequestParam("lastNameArabic") String lastNameArabic,
											@RequestParam("storeNameArabic") String storeNameArabic, @RequestParam("buildingArabic") String buildingArabic,
											@RequestParam("blockArabic") String blockArabic, @RequestParam("streetArabic") String streetArabic,
											@RequestParam("firstNameEnglish") String firstNameEnglish, @RequestParam("lastNameEnglish") String lastNameEnglish,
											@RequestParam("storeNameEnglish") String storeNameEnglish, @RequestParam("buildingEnglish") String buildingEnglish,
											@RequestParam("blockEnglish") String blockEnglish, @RequestParam("streetEnglish") String streetEnglish,@RequestParam("phoneNumber") String phoneNumber,
											@RequestParam("countryId") Long countryId, @RequestParam("cityId") Long cityId, @RequestParam("areaId") Long areaId,
											@RequestParam("latitude") BigDecimal latitude, @RequestParam("longitude") BigDecimal longitude, @RequestParam("preferredLanguage") String preferredLanguage,
											@RequestParam("isAdmin") Boolean isAdmin, @RequestParam("storeImage") MultipartFile storeImage, @RequestParam("featuredImage") MultipartFile featuredImage,
											@RequestParam("storeDetailImage") MultipartFile storeDetailImage) throws ValidationException, NotFoundException, FileOperationException {
		VendorDTO vendorDTO = VendorDTO.builder().id(id).areaId(areaId).blockArabic(blockArabic).blockEnglish(blockEnglish)
				.buildingArabic(buildingArabic).buildingEnglish(buildingEnglish).businessCategoryId(businessCategoryId).cityId(cityId)
				.countryId(countryId).email(email).firstNameArabic(firstNameArabic).firstNameEnglish(firstNameEnglish).isAdmin(isAdmin).lastNameArabic(lastNameArabic)
				.lastNameEnglish(lastNameEnglish).latitude(latitude).longitude(longitude).password(password).phoneNumber(phoneNumber).preferredLanguage(preferredLanguage)
				.storeNameArabic(storeNameArabic).storeNameEnglish(storeNameEnglish).streetArabic(streetArabic).streetEnglish(streetEnglish).build();
		VendorResponseDTO vendorResponseDTO = vendorService.addVendor(vendorDTO, storeImage, featuredImage, storeDetailImage);
		if (vendorDTO.getIsAdmin().booleanValue()) {
			userLoginService.sendWelComeEmail(vendorResponseDTO.getUserId());
		} else {
			vendorService.sendOtpForEmailVerification(vendorResponseDTO);
		}
		LOGGER.info("Outside add Vendor ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.create.message", null))
				.create();
	}

	/**
	 * Update vendor's personal details
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
	public ResponseEntity<Object> updatePersonalDetails(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final VendorDTO vendorDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update vendor {}", vendorDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		/**
		 * Added to handle the country,city in this, default it will be 1
		 */
		vendorDTO.setCountryId(1L);
		vendorDTO.setCityId(1L);
		vendorService.updatePersonalDetails(vendorDTO);
		LOGGER.info("Outside update vendor");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update bank details
	 *
	 * @param  vendorDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/bank/details")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
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
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileOperationException
	 */
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@PutMapping("/restaurant/details")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
	public ResponseEntity<Object> updateRestaurantDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "storeImage", required = false) final MultipartFile storeImage,
			@RequestParam(name = "storeDetailImage", required = false) final MultipartFile storeDetailImage,
			@RequestParam(name = "featuredImage", required = false) final MultipartFile featuredImage,
			@ModelAttribute @Valid final VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO, final BindingResult result)
			throws ValidationException, NotFoundException, FileOperationException {
		LOGGER.info("Inside update restaurant details {}", vendorRestaurantDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		vendorService.updateRestaurantDetails(vendorRestaurantDetailsDTO, storeImage, storeDetailImage, featuredImage);
		LOGGER.info("Outside restaurant account details ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Update subscription plan
	 *
	 * @param  accessToken
	 * @param  vendorBankDetailsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/{vendorId}/subscription/{subscriptionPlanId}")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
	public ResponseEntity<Object> updateSubscriptionPlan(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @PathVariable("subscriptionPlanId") final Long subscriptionPlanId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside update subscription plan for vendor:{} and planId:{}", vendorId, subscriptionPlanId);
		String url = vendorService.updateSubscriptionPlanForVendor(vendorId, subscriptionPlanId);
		LOGGER.info("Outside update subscription plan with url :{}", url);
		return new GenericResponseHandlers.Builder().setData(url).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null)).create();
	}

	/**
	 * Update order service enable or not for vendor
	 *
	 * @param  accessToken
	 * @param  vendorBankDetailsDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/orderservice/enable/{vendorId}")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
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
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{vendorId}")
	public ResponseEntity<Object> getVendor(@PathVariable("vendorId") final Long vendorId,
			@RequestParam(name = "isAdmin", required = false) final Boolean isAdmin) throws NotFoundException {
		LOGGER.info("Inside get Vendor for id:{}", vendorId);
		final VendorResponseDTO resultVendorResponseDTO = vendorService.getVendor(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_DETAIL_MESSAGE, null))
				.setData(resultVendorResponseDTO).create();
	}

	/**
	 * Get Vendor Bank details
	 *
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/bank/details/{vendorId}")
	public ResponseEntity<Object> getVendorBankDetails(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId)
			throws NotFoundException {
		LOGGER.info("Inside get Vendor for id:{}", vendorId);
		final VendorBankDetails resultVendorResponseDTO = vendorService.getVendorBankDetails(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_DETAIL_MESSAGE, null))
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
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_LIST_MESSAGE, null))
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
	@PreAuthorize("hasPermission('Vendor','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of Vendor of id {} and status {}", vendorId, active);
		String userName = vendorService.changeStatus(vendorId, active);
		if (userName != null) {
			revokeToken(userName.concat("!!").concat(UserType.VENDOR.name()));
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * revoke token for the user
	 *
	 * @param userName
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
	@PutMapping("/app/list/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getVendorCustomerListBasedOnParams(@RequestBody @Valid final VendorListFilterDTO vendorListFilterDTO,
			final BindingResult result, @PathVariable final Integer pageNumber, @PathVariable final Integer pageSize)
			throws ValidationException, NotFoundException {
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error(VENDOR_VALIDATION_FAILED);
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long totalCount = vendorService.getVendorCountForCustomerBasedOnParams(vendorListFilterDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);

		List<VendorAppResponseDTO> vendorList = vendorService.getVendorListForApp(vendorListFilterDTO, paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_LIST_MESSAGE, null))
				.setData(vendorList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}

	/**
	 * Change status of vendor
	 *
	 * @param  accessToken
	 * @param  vendorId
	 * @param  newStatus
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/change/status/{vendorId}/{newStatus}")
	@PreAuthorize("hasPermission('Vendor','CAN_DELETE')")
	public ResponseEntity<Object> changeVendorStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId,
			@PathVariable("newStatus") final String newStatus) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of Vendor of id {} and status {}", vendorId, newStatus);
		String userName = vendorService.changeVendorStatus(vendorId, newStatus);
		/**
		 * revoke token
		 */
		if (userName != null && (VendorStatus.SUSPENDED.getStatusValue().equals(newStatus) || VendorStatus.EXPIRED.getStatusValue().equals(newStatus))) {
			revokeToken(userName.concat("!!").concat(UserType.VENDOR.name()));
		}

		if (VendorStatus.ACTIVE.getStatusValue().equals(newStatus) || VendorStatus.APPROVED.getStatusValue().equals(newStatus)
				|| VendorStatus.REJECTED.getStatusValue().equals(newStatus) || VendorStatus.SUSPENDED.getStatusValue().equals(newStatus)
				|| VendorStatus.EXPIRED.getStatusValue().equals(newStatus)) {
			vendorService.sendEmailForChangeVendorStatus(vendorId);
		}
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * @param  accessToken
	 * @param  httpServletResponse
	 * @param  vendorFilterDTO
	 * @return
	 * @throws IOException
	 * @throws FileNotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/export/list")
	public ResponseEntity<Object> exportList(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse,
			@RequestBody final VendorFilterDTO vendorFilterDTO) throws FileNotFoundException, ValidationException {
		vendorService.exportVendorList(vendorFilterDTO, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_LIST_MESSAGE, null)).create();
	}

	/**
	 * update vendor is featured
	 *
	 * @param  accessToken
	 * @param  productId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/featured/{vendorId}")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
	public ResponseEntity<Object> changeStatusOfIsFeaturedProduct(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId, @RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of is featured vendor for id {} and new status {}", vendorId, active);
		vendorService.changeStatusOfIsFeaturedVendor(vendorId, active);
		LOGGER.info("Outside change status of is featured vendor ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * to delete image by type
	 *
	 * @param  accessToken
	 * @param  imageType
	 * @param  productId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@DeleteMapping("/image/{vendorId}")
	@PreAuthorize("hasPermission('Vendor','CAN_EDIT')")
	public ResponseEntity<Object> deleteImage(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "imageType", required = true) final String imageType, @PathVariable("vendorId") final Long vendorId)
			throws ValidationException, NotFoundException {
		vendorService.deleteVendorImageByType(vendorId, imageType);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * get vendor basic details
	 *
	 * @param  accessToken
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */

	@GetMapping("/basic/{vendorId}")
	public ResponseEntity<Object> getVendorBasicDetails(@RequestHeader("Authorization") final String accessToken, @PathVariable("vendorId") final Long vendorId)
			throws NotFoundException {
		LOGGER.info("Inside get Vendor basic details for id:{}", vendorId);
		final VendorBasicDetailDTO vendorBasicDetailDTO = vendorService.getVendorBasicDetailById(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_DETAIL_MESSAGE, null))
				.setData(vendorBasicDetailDTO).create();
	}

	/**
	 * redirect api from hesabe for vendor subscription. data is response from hesabe in encrypted form
	 *
	 * @param  data
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping(path = "/subscription/hesabe")
	public ModelAndView checkSubscriptionPaymentHesabe(@RequestParam(name = "data") final String data) {
		String result = hesabePaymentService.decrypt(data);
		LOGGER.info("hesabe response {} ", result);
		Gson gson = new Gson();
		HesabePaymentResponseDTO hesabePaymentResponseDTO = gson.fromJson(result, HesabePaymentResponseDTO.class);
		HesabeDecryptPaymentDTO decryptPaymentDTO = gson.fromJson(hesabePaymentResponseDTO.getResponse().get("data"), HesabeDecryptPaymentDTO.class);
		boolean response;
		String msg;
		try {
			response = vendorService.checkPaymentTransactionHesabe(decryptPaymentDTO.getResponse());
			msg = messageByLocaleService.getMessage("payment.success", null);
		} catch (NotFoundException | ValidationException e) {
			response = false;
			msg = e.getMessage();
		}
		if (response) {
			return new ModelAndView(REDIRECT + adminUrl + "auth/thank-you?message=" + msg + " &type=" + SuccessErrorType.PAYMENT);
		} else {
			return new ModelAndView(REDIRECT + adminUrl + "auth/error?message=" + msg + " &type=" + SuccessErrorType.PAYMENT);
		}
	}

	/**
	 * get subscription history
	 *
	 * @param  accessToken
	 * @param  vendorId
	 * @return
	 */
	@GetMapping("/subscription/history/{vendorId}")
	public ResponseEntity<Object> getVendorPaymentHistory(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId) {
		LOGGER.info("Inside get Vendor payment history for id:{}", vendorId);
		final List<VendorPaymentResponseDTO> vendorBasicDetailDTO = vendorPaymentService.getVendorPaymentListByVendorId(vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("vendor.payment.list.message", null))
				.setData(vendorBasicDetailDTO).create();
	}

	/**
	 * get detail for app
	 *
	 * @param  vendorId
	 * @param  vendorListFilterDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/app/detail/{vendorId}")
	public ResponseEntity<Object> getVendorCustomerDetailsBasedOnParams(@PathVariable("vendorId") final Long vendorId,
			@RequestParam(name = "latitude", required = false) final BigDecimal latitude,
			@RequestParam(name = "longitude", required = false) final BigDecimal longitude,
			@RequestParam(name = "customerAddressId", required = false) final Long customerAddressId) throws ValidationException, NotFoundException {
		VendorListFilterDTO vendorListFilterDTO = new VendorListFilterDTO();
		vendorListFilterDTO.setLatitude(latitude);
		vendorListFilterDTO.setLongitude(longitude);
		vendorListFilterDTO.setCustomerAddressId(customerAddressId);
		VendorResponseDTO vendor = vendorService.getVendorDetailForApp(vendorId, vendorListFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_DETAIL_MESSAGE, null))
				.setData(vendor).create();

	}

	@PutMapping("/admin/email/verify/{vendorId}")
	public ResponseEntity<Object> emailVerificationByAdmin(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("vendorId") final Long vendorId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside email verification of vendor by admin for id {} ", vendorId);
		Long userId = vendorService.verifyEmailByAdmin(vendorId);
		LOGGER.info("Outside email verification of vendor by admin");
		/**
		 * send welcome email
		 */
		userLoginService.sendWelComeEmail(userId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_UPDATE_MESSAGE, null))
				.create();
	}

	/**
	 * Get all vendor count
	 *
	 * @param  accessToken
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  sortByDirection
	 * @param  sortByField
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/count")
	public ResponseEntity<Object> getAllVendorCount(@RequestHeader("Authorization") final String accessToken) {
		LOGGER.info("Inside get all vendor count");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_DETAIL_MESSAGE, null))
				.setData(vendorService.getAllVendorCount()).create();
	}

	/**
	 * Get all the featured vendors
	 *
	 * @param accessToken
	 * @param latitude
	 * @param longitude
	 * @return
	 */
	@PostMapping("/app/list/featured")
	public ResponseEntity<Object> getAllFeaturedVendors(@RequestHeader("Authorization") final String accessToken,
														@RequestBody final CoordinatesDTO coordinatesDTO) throws ValidationException {
		LOGGER.info("Inside get all featured vendors");
		List<VendorAppResponseDTO> vendorList = vendorService.getAllFeaturedVendors(coordinatesDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(VENDOR_LIST_MESSAGE, null))
				.setData(vendorList).create();
	}
}
