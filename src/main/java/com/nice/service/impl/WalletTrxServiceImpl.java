package com.nice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.WalletTrxDTO;
import com.nice.exception.NotFoundException;
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
		wallet.setDescription(walletTrxDTO.getDescription());
		walletTrxRepository.save(wallet);
	}

	@Override
	public Page<WalletTrx> getWalletTrxList(final Integer pageNumber, final Integer pageSize, final Long customerId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (customerId != null) {
			return walletTrxRepository.findAllByCustomer(customerService.getCustomerDetails(customerId), pageable);
		} else {
			return walletTrxRepository.findAll(pageable);
		}
	}

}
