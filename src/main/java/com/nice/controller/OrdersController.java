/**
 *
 */
package com.nice.controller;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.GetMapping;
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
import com.nice.constant.PaymentMode;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.RefundAmountDto;
import com.nice.dto.ReplaceCancelOrderDto;
import com.nice.dto.WalletTrxDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Orders;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.OrdersService;
import com.nice.util.PaginationUtil;

/**
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

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * @param token
	 * @param orderRequestDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/placeOrder")
	@PreAuthorize("hasPermission('Orders','CAN_ADD')")
	public ResponseEntity<Object> placeOrder(@RequestHeader("Authorization") final String token, @Valid @RequestBody final OrderRequestDTO orderRequestDto,
			final BindingResult bindingResult) throws ValidationException, NotFoundException {
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		String orderId = orderService.validateOrder(orderRequestDto);
		LOGGER.info("Inside the validate order method OrderRequestDTO:{}", orderRequestDto);
		/**
		 * send email to customer when he/she place order and payment type is cod
		 */
		if (orderRequestDto.getPaymentMode().equalsIgnoreCase(PaymentMode.COD.name())) {
			/**
			 * Send Place order email
			 */
			orderService.sendEmailNotificationForOrder(NotificationQueueConstants.PLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER, Long.valueOf(orderId));
			/**
			 * Send Push Notification
			 */
			orderService.sendPushNotificationForOrder(NotificationQueueConstants.PLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER, Long.valueOf(orderId));
			orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.NEW_ORDER_PUSH_NOTIFICATION, Long.valueOf(orderId));
			orderService.sendEmailNotificationForOrder(NotificationQueueConstants.PLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER, Long.valueOf(orderId));

		}
		return new GenericResponseHandlers.Builder().setData(orderId).setMessage(messageByLocaleService.getMessage("order.create.successful", null))
				.setStatus(HttpStatus.OK).create();
	}

	/**
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

	@PostMapping("/customer/list")
	public ResponseEntity<Object> getCustomerOrderList(@RequestHeader("Authorization") final String token,
			@RequestBody final OrderListFilterDto orderListFilterDto) throws ValidationException, NotFoundException {
		LOGGER.info("Inside get order list method");

		final List<OrdersResponseDTO> orderList = orderService.getOrderListBasedOnParams(null, null, orderListFilterDto);

		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(ORDER_LIST_MESSAGE, null))
				.setData(orderList).create();
	}

	/**
	 * @param accessToken
	 * @param orderListFilterDto
	 * @param httpServletResponse
	 * @return
	 * @throws IOException
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileNotFoundException
	 */
	@PostMapping(value = "/export/list", produces = "text/csv")
	public ResponseEntity<Object> exportOrderList(@RequestHeader("Authorization") final String accessToken,
			@RequestBody final OrderListFilterDto orderListFilterDto, final HttpServletResponse httpServletResponse)
			throws NotFoundException, FileNotFoundException, ValidationException {
		orderService.exportOrderList(httpServletResponse, orderListFilterDto);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(ORDER_LIST_MESSAGE, null)).create();
	}

	/**
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

	/**
	 * @param token
	 * @param replaceCancelOrderDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/cancel")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> cancelOrder(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside the cancel order method");
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		Orders order = orderService.getOrderById(replaceCancelOrderDto.getOrderId());
		if (!OrderStatusEnum.PENDING.getStatusValue().equals(order.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("only.pending.order.cancel", null));
		}
		orderService.cancelOrder(replaceCancelOrderDto, true);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER, order.getId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER, order.getId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("cancel.order.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 * @param token
	 * @param replaceCancelOrderDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/admin/cancel")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> cancelOrderAdmin(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside the cancel order method of Admin");
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		orderService.cancelOrder(replaceCancelOrderDto, false);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_VENDOR,
				replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_BY_ADMIN_PUSH_NOTIFICATION_CUSTOMER,
				replaceCancelOrderDto.getOrderId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_DELIVERY_BOY,
				replaceCancelOrderDto.getOrderId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("cancel.order.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

	@PostMapping("/admin/replace/cancel")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> cancelReplaceReturnOrderAdmin(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside the cancel order method of Admin");
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		orderService.cancelReturnReplaceOrder(replaceCancelOrderDto);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_VENDOR,
				replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_BY_ADMIN_PUSH_NOTIFICATION_CUSTOMER,
				replaceCancelOrderDto.getOrderId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_DELIVERY_BOY,
				replaceCancelOrderDto.getOrderId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("cancel.order.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

	@PostMapping("/replace/cancel")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> cancelReplaceReturnOrder(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside the cancel order method");
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}

		Orders order = orderService.getOrderById(replaceCancelOrderDto.getOrderId());
		if (!(OrderStatusEnum.REPLACE_REQUESTED.getStatusValue().equals(order.getOrderStatus())
				|| OrderStatusEnum.RETURN_REQUESTED.getStatusValue().equals(order.getOrderStatus()))) {
			throw new ValidationException(messageByLocaleService.getMessage("only.pending.order.cancel", null));
		}
		orderService.cancelReturnReplaceOrder(replaceCancelOrderDto);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_PUSH_NOTIFICATION_CUSTOMER, order.getId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.CANCEL_ORDER_EMAIL_NOTIFICATION_CUSTOMER, order.getId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("cancel.order.success", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 * replace order
	 *
	 * @param token
	 * @param userId
	 * @param replaceCancelOrderDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/replace")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> replaceOrder(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		LOGGER.info("Inside the replace order method");
		orderService.replaceOrder(replaceCancelOrderDto);
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_VENDOR,
				replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.REPLACE_ORDER_PUSH_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.REPLACE_ORDER_EMAIL_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("replace.request.placed", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 * return order
	 *
	 * @param token
	 * @param userId
	 * @param replaceCancelOrderDto
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping("/return")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> returnOrder(@RequestHeader("Authorization") final String token,
			@Valid @RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto, final BindingResult bindingResult)
			throws ValidationException, NotFoundException {
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		LOGGER.info("Inside the return order method");
		orderService.returnOrder(replaceCancelOrderDto);
		orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_VENDOR,
				replaceCancelOrderDto.getOrderId());
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.RETURN_ORDER_PUSH_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.RETURN_ORDER_EMAIL_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		return new GenericResponseHandlers.Builder().setMessage(messageByLocaleService.getMessage("return.request.placed", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 * Change status of order </br>
	 * This API is useful for CONFIRMED,REJECT,ORDER_IS_READY,RETURN_PROCESSED,REPLACE-PROCESSED
	 *
	 * @param accessToken
	 * @param userId
	 * @param orderId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/{ordersId}/status")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("ordersId") final Long ordersId,
			@RequestParam("status") final String status) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of order for id {} and new status {}", ordersId, status);
		if (OrderStatusEnum.CANCELLED.getStatusValue().equalsIgnoreCase(status) || OrderStatusEnum.REJECTED.getStatusValue().equalsIgnoreCase(status)) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.order.status", null));
		}
		orderService.changeStatus(ordersId, status);
		if (!OrderStatusEnum.DELIVERED.getStatusValue().equals(status)) {
			orderService.sendPushNotificationForOrder(NotificationQueueConstants.ORDER_STATUS_CHANGE_PUSH_NOTIFICATION_CUSTOMER, ordersId);
		}
		if (OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue().equals(status) || OrderStatusEnum.REPLACE_ORDER_PREPARED.getStatusValue().equals(status)) {
			orderService.sendPushNotificationToVendorOrDeliveryBoy(NotificationQueueConstants.ORDER_PREPARED, ordersId);
		}
		LOGGER.info("Outside change status of order");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.change.status.messege", null))
				.create();

	}

	@PutMapping("/reject")
	@PreAuthorize("hasPermission('Orders','CAN_EDIT')")
	public ResponseEntity<Object> rejectOrderByVendor(@RequestHeader("Authorization") final String accessToken,
			@RequestBody final ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException {
		LOGGER.info("Inside reject order for id {}", replaceCancelOrderDto.getOrderId());
		orderService.rejectOrder(replaceCancelOrderDto);
		LOGGER.info("After rejected order");
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.REJECT_ORDER_PUSH_NOTIFICATION_CUSTOMER, replaceCancelOrderDto.getOrderId());
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.rejected", null)).create();

	}

	@GetMapping("/next/status/{orderId}")
	public ResponseEntity<Object> getNextStatusForOrder(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long orderId)
			throws NotFoundException {
		LOGGER.info("Inside get next status for id {}", orderId);
		List<String> nextStatus = orderService.getNextStatus(orderId);
		LOGGER.info("After get next status for id {}", orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("next.status.displayed.successfully", null)).setData(nextStatus).create();

	}

	/**
	 * Retry for searching delivery boys for assignment of order
	 *
	 * @param accessToken
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/{orderId}/retry")
	public ResponseEntity<Object> retryForSearchingDeliveryBoys(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long orderId)
			throws NotFoundException, ValidationException {
		LOGGER.info("retry for searching delivery boys, orderId:{} ", orderId);
		orderService.retryForSearchingDeliveryBoys(orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.update.message", null))
				.create();
	}

	/**
	 * This method is used to refund amount for the orders that are cancelled by admin, as for orders cancelled by admin no
	 * refund would be made automatically, for other type of cancelled order (Cancelled By Customer/Rejected by Vendor)
	 * refund would be made automatically if the payment mode is not COD
	 *
	 * @param accessToken
	 * @param orderId
	 * @param amount
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/refund/amount")
	public ResponseEntity<Object> refundAmountForCancelOrders(@RequestHeader("Authorization") final String accessToken,
			@Valid @RequestBody final RefundAmountDto refundAmountDto, final BindingResult bindingResult) throws NotFoundException, ValidationException {
		LOGGER.info("Inside refund amount for orderId:{}, amount :{} ", refundAmountDto.getOrderId(), refundAmountDto.getAdminContribution());
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		String description = refundAmountDto.getDescription();
		if (description != null && description.length() > 255) {
			throw new ValidationException(messageByLocaleService.getMessage("description.max.length", null));

		}
		orderService.refundAmount(refundAmountDto);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.REFUND_ORDER_PUSH_NOTIFICATION_CUSTOMER, refundAmountDto.getOrderId());
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.update.message", null))
				.create();
	}

	/**
	 * This method will only be used to deliver pickup type order by vendor, for all other orders the delivery would be done
	 * by delivery boy.
	 *
	 * @param token
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PutMapping("/deliver/pickup/order/{orderId}")
	public ResponseEntity<Object> deliverPickupOrderByVendor(@RequestHeader("Authorization") final String token, @PathVariable final Long orderId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get order details method for orderId : {}", orderId);
		orderService.deliverPickUpOrder(orderId);
		orderService.sendPushNotificationForOrder(NotificationQueueConstants.DELIVER_ORDER_PUSH_NOTIFICATION_CUSTOMER, orderId);
		orderService.sendEmailNotificationForOrder(NotificationQueueConstants.DELIVER_ORDER_EMAIL_NOTIFICATION_CUSTOMER, orderId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("order.delivered.successful", null))
				.create();
	}

	/**
	 * Gives the details of current ongoing orders for a customer.
	 *
	 * @param token
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/ongoing")
	public ResponseEntity<Object> checkForOngoingOrder(@RequestHeader("Authorization") final String token) throws ValidationException, NotFoundException {
		OrdersResponseDTO ordersResponseDto = orderService.getOngoingOrderForCustomer();
		return new GenericResponseHandlers.Builder().setData(ordersResponseDto).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("ongoing.order.display", null)).create();

	}

	@GetMapping("/{orderId}/refund/detail")
	public ResponseEntity<Object> checkForOngoingOrder(@RequestHeader("Authorization") final String token, @PathVariable final Long orderId)
			throws ValidationException, NotFoundException {
		WalletTrxDTO walletTxnDto = orderService.getRefundDetailsForOrder(orderId);
		return new GenericResponseHandlers.Builder().setData(walletTxnDto).setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("refund.details.display", null)).create();

	}
}
