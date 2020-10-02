package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.PaymentMode;
import com.nice.constant.Role;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.PaymentTransactionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.UserLogin;
import com.nice.service.OrdersService;
import com.nice.service.PaymentTransactionService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@Service(value = "paymentTransactionService")
@Transactional(rollbackFor = Throwable.class)
public class PaymentTransactionServiceImpl implements PaymentTransactionService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private OrdersService ordersService;

	@Override
	public List<PaymentTransactionDTO> getPaymentTransactionList(final OrderListFilterDto orderListFilterDto, final Integer startIndex, final Integer pageSize)
			throws NotFoundException, ValidationException {
		List<OrdersResponseDTO> ordersList;
		orderListFilterDto.setIsForPaymentTransaction(true);
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole().getName()) || Role.VENDOR.getStatusValue().equals(userLogin.getRole().getName())) {
			ordersList = ordersService.getOrderListBasedOnParams(startIndex, pageSize, orderListFilterDto);
			return convertToPaymentTransactionList(ordersList);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.role.message", new Object[] { userLogin.getRole().getName() }));
		}
	}

	/**
	 * @param orderListFilterDto
	 * @param ordersList
	 * @return
	 */
	private List<PaymentTransactionDTO> convertToPaymentTransactionList(final List<OrdersResponseDTO> ordersList) {
		List<PaymentTransactionDTO> paymentTransactionDTOs = new ArrayList<>();
		for (OrdersResponseDTO order : ordersList) {
			PaymentTransactionDTO paymentTransactionDTO = new PaymentTransactionDTO();
			paymentTransactionDTO.setOrderId(order.getId());
			paymentTransactionDTO.setAmountPaid(order.getTotalOrderAmount());
			paymentTransactionDTO.setCustomerName(order.getCustomerName());
			paymentTransactionDTO.setCustomerId(order.getCustomerId());
			paymentTransactionDTO.setVendorName(order.getVendorName());
			paymentTransactionDTO.setVendorId(order.getVendorId());
			paymentTransactionDTO.setCustomerEmail(order.getEmail());
			paymentTransactionDTO.setPaymentMode(order.getPaymentMode());
			if (PaymentMode.COD.name().equals(order.getPaymentMode())) {
				paymentTransactionDTO.setPaymentDate(order.getDeliveryDate());
			} else if (PaymentMode.ONLINE.name().equals(order.getPaymentMode())) {
				paymentTransactionDTO.setPaymentDate(order.getCreatedAt());
				paymentTransactionDTO.setTransactionId(order.getPaymentId());
			}
			if (order.getDeliveryBoyId() != null) {
				paymentTransactionDTO.setDeliveryBoyId(order.getDeliveryBoyId());
				paymentTransactionDTO.setDeliveryBoyName(order.getDeliveryBoyName());
			}
			paymentTransactionDTOs.add(paymentTransactionDTO);
		}
		return paymentTransactionDTOs;
	}
}
