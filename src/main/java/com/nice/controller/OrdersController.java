/**
 *
 */
package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.nice.constant.PaymentMode;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.ReplaceCancelOrderDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OrdersService;
import com.nice.util.PaginationUtil;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
@RequestMapping(path = "/order")
@RestController
public class OrdersController {

	/**
	 *
	 */
	private static final String ORDER_LIST_MESSAGE = "order.list.message";

	private static final Logger LOGGER = LoggerFactory.getLogger(OrdersController.class);

	@Autowired
	private OrdersService orderService;

	@Value("${customer.url}")
	private String customerUrl;

	private static final String REDIRECT = "redirect:";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 *
	 * @param token
	 * @param orderRequestDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/placeOrder")
	public ResponseEntity<Object> placeOrder(@RequestHeader("Authorization") final String token, @Valid @RequestBody final OrderRequestDTO orderRequestDto,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		String orderId = orderService.validateOrder(orderRequestDto);
		LOGGER.info("Inside the validate order method");
		/**
		 * send email start here
		 */
		/**
		 * send email to customer when he/she place order and payment type is cod
		 */
		if (orderRequestDto.getPaymentMode().equalsIgnoreCase(PaymentMode.COD.name())) {
			// TODO
			/**
			 * Uncomment and modify once clarity on notification
			 */
			// orderService.sendEmailOnOrderStatusChange(Long.valueOf(orderId), NotificationQueueConstants.PLACE_ORDER);
			// orderService.sendPushNotificationOnStatus(Long.valueOf(orderId), NotificationQueueConstants.PLACE_ORDER);
		} /**
			 * send email ends here
			 */
		return new GenericResponseHandlers.Builder().setData(orderId).setMessage("Order Placed Successfully").setStatus(HttpStatus.OK).create();
	}

	/**
	 *
	 * @param token
	 * @param pageNumber
	 * @param pageSize
	 * @param orderListFilterDto
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getOrderList(@RequestHeader("Authorization") final String token, @PathVariable final Integer pageNumber,
			@PathVariable final Integer pageSize, @RequestBody final OrderListFilterDto orderListFilterDto) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get order list method");

		Long totalCount = orderService.getOrderCountBasedOnParams(orderListFilterDto);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);

		final List<OrdersResponseDTO> orderList = orderService.getOrderListBasedOnParams(paginationUtilDto.getStartIndex(), pageSize, orderListFilterDto);

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(ORDER_LIST_MESSAGE, null))
				.setData(orderList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount).setTotalPages(paginationUtilDto.getTotalPages().intValue())
				.create();
	}

	/**
	 *
	 * @param token
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{orderId}")
	public ResponseEntity<Object> getOrderDetails(@RequestHeader("Authorization") final String token, @PathVariable final Long orderId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get order details method for orderId : {}", orderId);
		final OrdersResponseDTO orderDetails = orderService.getOrderDetails(orderId);

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.detail.message", null))
				.setData(orderDetails).create();
	}

	@PutMapping("/app/razorpay/{razorPayOrderId}")
	public ModelAndView createRazorPayFormForApp(@RequestHeader("Authorization") final String token,
			@PathVariable("razorPayOrderId") final String razorPayOrderId, @RequestHeader final Long userId) {
		return new ModelAndView(REDIRECT + customerUrl + "#/app-payment/'" + razorPayOrderId + "'");
	}

	/**
	 *
	 * @param token
	 * @param replaceCancelOrderDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/cancel")
	public ResponseEntity<Object> cancelOrder(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside the cancel order method");
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		orderService.cancelOrder(replaceCancelOrderDto);
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("cancel.order.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

}
