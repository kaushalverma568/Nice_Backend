package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.UserType;
import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PaymentDetailsMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.PaymentDetails;
import com.nice.model.Task;
import com.nice.model.Vendor;
import com.nice.repository.PaymentDetailsRepository;
import com.nice.repository.TaskRepository;
import com.nice.service.PaymentDetailsService;
import com.nice.service.TaskService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("paymentDetailsService")
public class PaymentDetailsServiceImpl implements PaymentDetailsService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private PaymentDetailsRepository paymentDetailsRepository;

	@Autowired
	private PaymentDetailsMapper paymentDetailsMapper;

	@Autowired
	private TaskService taskService;

	@Autowired
	private TaskRepository taskRepository;

	@Override
	public void addPaymentDetails(final PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException {
		List<Task> taskList = new ArrayList<>();

		for (Long taskId : paymentDetailsDTO.getTaskIds()) {
			taskList.add(taskService.getTaskDetail(taskId));
		}
		PaymentDetails paymentDetails = paymentDetailsMapper.toEntity(paymentDetailsDTO);

		/**
		 * if tasks do not have unique delivery boy OR vendor then throw exception
		 */
		if (UserType.DELIVERY_BOY.name().equals(paymentDetailsDTO.getEntityType())) {
			List<DeliveryBoy> deliveryBoys = taskList.stream().map(Task::getDeliveryBoy).distinct().collect(Collectors.toList());
			if (!deliveryBoys.isEmpty() && deliveryBoys.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.delivery.boy", null));
			}
			/**
			 * get task list calculate amount and validate it
			 */
			Double sum = 0d;
			for (Task task : taskList) {
				sum += task.getDeliveryCharge();
			}
			if (!sum.equals(paymentDetailsDTO.getPaymentAmount())) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.amount", new Object[] { sum }));
			}
			paymentDetails.setDeliveryBoy(deliveryBoys.get(0));
		} else {
			List<Vendor> vendors = taskList.stream().map(Task::getVendor).distinct().collect(Collectors.toList());
			if (!vendors.isEmpty() && vendors.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("orders.belong.multiple.vendor", null));
			}
			/**
			 * get task list calculate amount and validate it
			 */
			Double sum = 0d;
			for (Task task : taskList) {
				sum += task.getVendorPayableAmt();
			}
			if (!sum.equals(paymentDetailsDTO.getPaymentAmount())) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.amount", new Object[] { sum }));
			}
			paymentDetails.setVendor(vendors.get(0));
		}
		/**
		 * Setting total no of orders based on task list here
		 */
		paymentDetails.setNoOfOrders(taskList.size());
		paymentDetails = paymentDetailsRepository.save(paymentDetails);

		/**
		 * Saving the payment details in task of delivery boy and vendor
		 */
		if (UserType.DELIVERY_BOY.name().equals(paymentDetailsDTO.getEntityType())) {
			for (Task task : taskList) {
				task.setDeliveryBoyPaymentDetails(paymentDetails);
			}
		} else {
			for (Task task : taskList) {
				task.setVendorPaymentDetails(paymentDetails);
			}
		}
		taskRepository.saveAll(taskList);
	}

	@Override
	public PaymentDetailsResponseDTO getPaymentDetails(final Long paymentDetailsId) throws NotFoundException {
		final PaymentDetails existingPaymentDetails = getPaymentDetailsDetail(paymentDetailsId);
		return paymentDetailsMapper.toDto(existingPaymentDetails);
	}

	@Override
	public PaymentDetails getPaymentDetailsDetail(final Long paymentDetailsId) throws NotFoundException {
		return paymentDetailsRepository.findById(paymentDetailsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("payment.details.not.found", new Object[] { paymentDetailsId })));
	}

	@Override
	public Boolean isPaymentDetailsExists(final PaymentDetailsDTO paymentDetailsDTO) {
		if (paymentDetailsDTO.getId() != null) {
			/**
			 * At the time of update is paymentDetails with same transaction no exist or not except it's own id
			 */
			return paymentDetailsRepository.findByTransactionNoIgnoreCaseAndIdNot(paymentDetailsDTO.getTransactionNo(), paymentDetailsDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is paymentDetails with same transaction no exist or not
			 */
			return paymentDetailsRepository.findByTransactionNoIgnoreCase(paymentDetailsDTO.getTransactionNo()).isPresent();
		}
	}

	@Override
	public List<PaymentDetails> getPaymentHistory(final Date fromDate, final Date toDate) {
		if (fromDate != null) {
			if (toDate != null) {
				return paymentDetailsRepository.findAllByPaidOnBetween(fromDate, toDate);
			} else {
				return paymentDetailsRepository.findAllByPaidOnGreaterThanEqual(fromDate);
			}
		} else {
			if (toDate != null) {
				return paymentDetailsRepository.findAllByPaidOnLessThanEqual(toDate);
			} else {
				return paymentDetailsRepository.findAll();
			}
		}
	}
}
