package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.PaymentDetails;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Repository
public interface PaymentDetailsRepository extends JpaRepository<PaymentDetails, Long> {

	/**
	 * Get PaymentDetails by paymentDetails transactionNo and paymentDetails Id not
	 * equal if exist
	 *
	 * @param transactionNo
	 * @param id
	 * @return
	 */
	Optional<PaymentDetails> findByTransactionNoIgnoreCaseAndIdNot(String transactionNo, Long id);

	/**
	 * Get PaymentDetails by paymentDetails transactionNo if exist
	 *
	 * @param transactionNo
	 * @return
	 */
	Optional<PaymentDetails> findByTransactionNoIgnoreCase(String transactionNo);

	/**
	 * find all by paidOn between from date and to date
	 *
	 * @param fromDate
	 * @param toDate
	 */
	List<PaymentDetails> findAllByPaidOnBetween(Date fromDate, Date toDate);

	/**
	 * find all by paidOn greater than from date
	 *
	 * @param fromDate
	 * @return
	 */
	List<PaymentDetails> findAllByPaidOnGreaterThanEqual(Date fromDate);

	/**
	 * find all by paidOn less than to date
	 *
	 * @param toDate
	 * @return
	 */
	List<PaymentDetails> findAllByPaidOnLessThanEqual(Date toDate);

}
