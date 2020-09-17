/**
 *
 */
package com.nice.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.DeliveryType;
import com.nice.dto.CheckOutDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.UserLogin;
import com.nice.service.CartItemService;
import com.nice.service.CheckOutService;
import com.nice.service.CustomerService;
import com.nice.service.OrdersService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Sep-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("checkOutService")
public class CheckOutServiceImpl implements CheckOutService {

	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private CustomerService customerService;

	@Override
	public CheckOutDTO getCheckOutPageDetails(final String deliveryType, final Boolean useWallet) throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		CheckOutDTO checkOutDto = new CheckOutDTO();
		checkOutDto.setDeliveryCharge(0.0d);
		checkOutDto.setWalletContribution(0.0d);
		checkOutDto.setCartItemResponseList(cartItemService.getCartItemDetailList());
		List<CartItem> cartItemList = cartItemService.getCartListBasedOnCustomer(userLogin.getEntityId());
		/**
		 * Calculate the total order amount for checkout
		 */
		Double totalOrderAmount;
		Double orderAmount = totalOrderAmount = ordersService.calculateTotalOrderAmt(cartItemList);
		checkOutDto.setGrossOrderAmount(orderAmount);
		if (DeliveryType.DELIVERY.getStatusValue().equals(deliveryType)) {
			Double orderAmountWithDeliveryCharge = ordersService.addDeliveryCharge(true, orderAmount);
			checkOutDto.setDeliveryCharge(Double.sum(orderAmountWithDeliveryCharge, orderAmount * (-1)));
			totalOrderAmount = orderAmountWithDeliveryCharge;
		}
		Double walletBalance = customerService.getWalletBalance();
		checkOutDto.setCustomerWalletAmount(walletBalance);
		if (useWallet.booleanValue()) {
			if (walletBalance >= totalOrderAmount) {
				checkOutDto.setWalletContribution(totalOrderAmount);
				totalOrderAmount = 0.0d;
			} else {
				checkOutDto.setWalletContribution(walletBalance);
				totalOrderAmount = Double.sum(totalOrderAmount, walletBalance * (-1));
			}
		}
		checkOutDto.setTotalOrderAmount(totalOrderAmount);
		return checkOutDto;
	}

}
