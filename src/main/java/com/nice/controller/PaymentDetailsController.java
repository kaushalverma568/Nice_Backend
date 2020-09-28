package com.nice.controller;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.ws.rs.Produces;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.PayableAmountDTO;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.dto.VendorPayoutDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PaymentDetailsMapper;
import com.nice.model.PaymentDetails;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.PaymentDetailsService;
import com.nice.util.PaginationUtil;
import com.nice.validator.PaymentDetailsValidator;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 15-07-2020
 */
@RequestMapping(path = "/payment/details")
@RestController
public class PaymentDetailsController {
	private static final String PAYMENT_DETAILS_DETAIL_MESSAGE = "payment.details.detail.message";
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PaymentDetailsController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * Validator - to apply/check any type of validation regarding paymentDetails
	 */
	@Autowired
	private PaymentDetailsValidator paymentDetailsValidator;

	/**
	 * to bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(paymentDetailsValidator);
	}

	@Autowired
	private PaymentDetailsService paymentDetailsService;

	@Autowired
	private PaymentDetailsMapper paymentDetailsMapper;

	/**
	 * Add PaymentDetails
	 *
	 * @param  paymentDetailsDTO
	 * @param  result
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	@PreAuthorize("hasPermission('Delivery Boy Payout','CAN_ADD')")
	public ResponseEntity<Object> addPaymentDetails(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final PaymentDetailsDTO paymentDetailsDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add PaymentDetails {}", paymentDetailsDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("PaymentDetails validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long paymentDetailsId = paymentDetailsService.addPaymentDetails(paymentDetailsDTO);
		LOGGER.info("Outside add PaymentDetails ");
		/**
		 * send email to delivery boy or vendor after payout
		 */
		paymentDetailsService.sendEmailAfterPayout(paymentDetailsDTO.getEntityType(), paymentDetailsId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("payment.details.create.message", null)).create();
	}

	/**
	 * Get PaymentDetails Details based on id
	 *
	 * @param  paymentDetailsId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/{paymentDetailsId}")
	public ResponseEntity<Object> getPaymentDetails(@RequestHeader("Authorization") final String accessToken,
			@PathVariable("paymentDetailsId") final Long paymentDetailsId) throws NotFoundException {
		LOGGER.info("Inside get PaymentDetails for id:{} ", paymentDetailsId);
		final PaymentDetailsResponseDTO resultPaymentDetailsDTO = paymentDetailsService.getPaymentDetails(paymentDetailsId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(PAYMENT_DETAILS_DETAIL_MESSAGE, null)).setData(resultPaymentDetailsDTO).create();
	}

	/**
	 * Get delivery boy payout
	 *
	 * @param  paymentDetailsId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/deliveryboy/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Delivery Boy Payout','CAN_VIEW')")
	public ResponseEntity<Object> getDeliveryBoyPayout(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "deliveryBoyId", required = false) final Long deliveryBoyId,
			@RequestParam(name = "registeredOn", required = false) final Date registeredOn, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize) throws ValidationException {
		LOGGER.info("Inside  Get delivery boy payout for deliveryBoyId:{},registeredOn:{} ", deliveryBoyId, registeredOn);
		Long totalCount = paymentDetailsService.getDeliveryBoyPayoutCountBasedOnParam(deliveryBoyId, registeredOn);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<DeliveryBoyPayoutDTO> resultPaymentDetailsDTO = paymentDetailsService.getDeliveryBoyPayout(deliveryBoyId, registeredOn,
				paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.payout.detail.message", null)).setData(resultPaymentDetailsDTO)
				.setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}

	/**
	 * export delivery boy payout
	 *
	 * @param  paymentDetailsId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	@GetMapping("/deliveryboy/export")
	@PreAuthorize("hasPermission('Delivery Boy Payout','CAN_VIEW')")
	public ResponseEntity<Object> exportDeliveryBoyPayout(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "deliveryBoyId", required = false) final Long deliveryBoyId,
			@RequestParam(name = "registeredOn", required = false) final Date registeredOn, final HttpServletResponse httpServletResponse)
			throws FileOperationException {
		LOGGER.info("Inside  export delivery boy payout for deliveryBoyId:{},registeredOn:{} ", deliveryBoyId, registeredOn);
		paymentDetailsService.exportDeliveryBoyPayout(deliveryBoyId, registeredOn, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("deliveryboy.payout.detail.message", null)).create();
	}

	/**
	 * Get vendor payout
	 *
	 * @param  paymentDetailsId
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/vendor/pageNumber/{pageNumber}/pageSize/{pageSize}")
	@PreAuthorize("hasPermission('Vendor Payout','CAN_VIEW')")
	public ResponseEntity<Object> getVendorPayout(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "vendorId", required = false) final Long vendorId,
			@RequestParam(name = "businessCategoryId", required = false) final Long businessCategoryId, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize) throws ValidationException {
		LOGGER.info("Inside  get vendor payout for vendorId:{},businessCategoryId:{} ", vendorId, businessCategoryId);
		Long totalCount = paymentDetailsService.getVendorPayoutCountBasedOnParam(vendorId, businessCategoryId);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<VendorPayoutDTO> resultPaymentDetailsDTO = paymentDetailsService.getVendorPayout(vendorId, businessCategoryId,
				paginationUtilDto.getStartIndex(), pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("vendor.payout.detail.message", null)).setData(resultPaymentDetailsDTO)
				.setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}

	/**
	 * Export vendor payout
	 *
	 * @param  accessToken
	 * @param  vendorId
	 * @param  businessCategoryId
	 * @param  httpServletResponse
	 * @return
	 * @throws FileOperationException
	 */
	@GetMapping("/vendor/export")
	@PreAuthorize("hasPermission('Vendor Payout','CAN_VIEW')")
	public ResponseEntity<Object> exportVendorPayout(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "vendorId", required = false) final Long vendorId,
			@RequestParam(name = "businessCategoryId", required = false) final Long businessCategoryId, final HttpServletResponse httpServletResponse)
			throws FileOperationException {
		LOGGER.info("Inside  export vendor payout for vendorId:{},businessCategoryId:{} ", vendorId, businessCategoryId);
		paymentDetailsService.exportVendorPayout(vendorId, businessCategoryId, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("vendor.payout.detail.message", null)).create();
	}

	/**
	 * Get payment History
	 *
	 * @param  accessToken
	 * @param  fromDate
	 * @param  toDate
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/history/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getPaymentHistory(@RequestHeader("Authorization") final String accessToken, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestParam(name = "fromDate", required = false) final Date fromDate,
			@RequestParam(name = "toDate", required = false) final Date toDate,
			@RequestParam(name = "deliveryBoyId", required = false) final Long deliveryBoyId,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get payment history for deliveryBoyId:{},vendorId:{}, fromDate:{} and toDate:{} ", deliveryBoyId, vendorId, fromDate, toDate);
		final Page<PaymentDetails> paymentHistory = paymentDetailsService.getPaymentHistory(deliveryBoyId, vendorId, fromDate, toDate, pageNumber, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(PAYMENT_DETAILS_DETAIL_MESSAGE, null))
				.setData(paymentDetailsMapper.toDtos(paymentHistory.getContent())).setHasNextPage(paymentHistory.hasNext())
				.setHasPreviousPage(paymentHistory.hasPrevious()).setTotalPages(paymentHistory.getTotalPages()).setPageNumber(paymentHistory.getNumber() + 1)
				.setTotalCount(paymentHistory.getTotalElements()).create();
	}

	/**
	 * export payment history
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  httpServletResponse
	 * @param  activeRecords
	 * @return
	 * @throws FileOperationException
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Produces("text/csv")
	@GetMapping("/export/history")
	public ResponseEntity<Object> exportPaymentHistory(@RequestHeader("Authorization") final String accessToken, final HttpServletResponse httpServletResponse,
			@RequestParam(name = "fromDate", required = false) final Date fromDate, @RequestParam(name = "toDate", required = false) final Date toDate,
			@RequestParam(name = "deliveryBoyId", required = false) final Long deliveryBoyId,
			@RequestParam(name = "vendorId", required = false) final Long vendorId) throws FileOperationException, ValidationException, NotFoundException {
		paymentDetailsService.exportPaymentHistory(httpServletResponse, deliveryBoyId, vendorId, fromDate, toDate);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(PAYMENT_DETAILS_DETAIL_MESSAGE, null)).create();
	}

	/**
	 * Get payable amount from task list
	 *
	 * @param  accessToken
	 * @param  fromDate
	 * @param  toDate
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/payable")
	public ResponseEntity<Object> getPayableAmountForTaskList(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final PayableAmountDTO payableAmountDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get payable amount for task list :{}", payableAmountDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("PaymentDetails validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		final Double amount = paymentDetailsService.getPayableAmountForTaskList(payableAmountDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage(PAYMENT_DETAILS_DETAIL_MESSAGE, null)).setData(amount).create();
	}
}