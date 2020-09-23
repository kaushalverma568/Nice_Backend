package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.UserType;
import com.nice.dto.DeliveryBoyPayoutDTO;
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
import com.nice.service.DeliveryBoyService;
import com.nice.service.PaymentDetailsService;
import com.nice.service.TaskService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
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

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private VendorService vendorService;

	@Override
	public void addPaymentDetails(final PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException, ValidationException {
		List<Task> taskList = new ArrayList<>();

		for (Long taskId : paymentDetailsDTO.getTaskIds()) {
			taskList.add(taskService.getTaskDetail(taskId));
		}
		PaymentDetails paymentDetails = paymentDetailsMapper.toEntity(paymentDetailsDTO);
		paymentDetails.setActive(true);
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
	public Page<PaymentDetails> getPaymentHistory(final Long deliveryBoyId, final Long vendorId, final Date fromDate, final Date toDate,
			final Integer pageNumber, final Integer pageSize) throws ValidationException, NotFoundException {
		if ((deliveryBoyId == null && vendorId == null) || (deliveryBoyId != null && vendorId != null)) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.or.vendor.required", null));
		}
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC, "id"));
		if (deliveryBoyId != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
			if (fromDate != null) {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnBetweenAndDeliveryBoy(fromDate, toDate, deliveryBoy, pageable);
				} else {
					return paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndDeliveryBoy(fromDate, deliveryBoy, pageable);
				}
			} else {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnLessThanEqualAndDeliveryBoy(toDate, deliveryBoy, pageable);
				} else {
					return paymentDetailsRepository.findAllByDeliveryBoy(deliveryBoy, pageable);
				}
			}
		} else {
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			if (fromDate != null) {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnBetweenAndVendor(fromDate, toDate, vendor, pageable);
				} else {
					return paymentDetailsRepository.findAllByPaidOnGreaterThanEqualAndVendor(fromDate, vendor, pageable);
				}
			} else {
				if (toDate != null) {
					return paymentDetailsRepository.findAllByPaidOnLessThanEqualAndVendor(toDate, vendor, pageable);
				} else {
					return paymentDetailsRepository.findAllByVendor(vendor, pageable);
				}
			}
		}
	}

	@Override
	public List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(final Long searchId, final Long deliveryBoyId, final Date registeredOn, final Integer startIndex,
			final Integer pageSize) {
		return paymentDetailsRepository.getDeliveryBoyPayout(searchId, deliveryBoyId, registeredOn, startIndex, pageSize);
	}

	@Override
	public Long getDeliveryBoyPayoutCountBasedOnParam(final Long searchId, final Long deliveryBoyId, final Date registeredOn) {
		return paymentDetailsRepository.getDeliveryBoyPayoutCountBasedOnParam(searchId, deliveryBoyId, registeredOn);
	}
}
