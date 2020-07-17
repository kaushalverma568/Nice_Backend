package com.nice.service.impl;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.PaymentDetailsDTO;
import com.nice.dto.PaymentDetailsResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PaymentDetailsMapper;
import com.nice.model.PaymentDetails;
import com.nice.repository.PaymentDetailsRepository;
import com.nice.service.PaymentDetailsService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 16-07-2020
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

	@Override
	public void addPaymentDetails(final PaymentDetailsDTO paymentDetailsDTO) throws NotFoundException {
		/**
		 * get task list from ids calculate amount and validate it and then set task
		 * payment status and payment details id
		 */
		PaymentDetails paymentDetails = paymentDetailsMapper.toEntity(paymentDetailsDTO);
		paymentDetailsRepository.save(paymentDetails);
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
			 * At the time of update is paymentDetails with same transaction no exist or not
			 * except it's own id
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
