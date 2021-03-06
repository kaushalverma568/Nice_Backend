package com.nice.service.impl;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.WalletTrxDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.mapper.WalletTrxMapper;
import com.nice.model.Customer;
import com.nice.model.Orders;
import com.nice.model.WalletTrx;
import com.nice.repository.WalletTrxRepository;
import com.nice.service.CustomerService;
import com.nice.service.OrdersService;
import com.nice.service.WalletTrxService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Service(value = "walletTrxService")
@Transactional(rollbackFor = Throwable.class)
public class WalletTrxServiceImpl implements WalletTrxService {

	@Autowired
	private WalletTrxRepository walletTrxRepository;

	@Autowired
	private WalletTrxMapper walletTrxMapper;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private OrdersService orderService;

	@Override
	public void addupdateWalletTrx(final WalletTrxDTO walletTrxDTO) throws NotFoundException {
		Customer customer = customerService.getCustomerDetails(walletTrxDTO.getCustomerId());
		Orders order = orderService.getOrderById(walletTrxDTO.getOrderId());
		WalletTrx wallet = walletTrxMapper.toEntity(walletTrxDTO);
		wallet.setCustomer(customer);
		wallet.setOrder(order);
		walletTrxRepository.save(wallet);
	}

	@Override
	public Page<WalletTrx> getWalletTrxList(final Integer pageNumber, final Integer pageSize, final Long customerId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC, "id"));
		if (customerId != null) {
			return walletTrxRepository.findByCustomerAndAmountNot(customerService.getCustomerDetails(customerId), 0.0d, pageable);
		} else {
			return walletTrxRepository.findAllByAmountNot(0.0d, pageable);
		}
	}

	@Override
	public WalletTrxDTO getWalletTxnByOrderIdAndTxnType(final Long orderId, final String transactionType) throws NotFoundException, ValidationException {

		Orders order = orderService.getOrderById(orderId);
		Optional<WalletTrx> walletTrx = walletTrxRepository.findByOrderAndTransactionType(order, transactionType);
		if (!walletTrx.isPresent()) {
			throw new ValidationException("order.not.refunded", null);
		}
		return walletTrxMapper.toDto(walletTrx.get());

	}

}
