package com.nice.repository;

import java.util.Date;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.PaymentDetails;
import com.nice.model.Vendor;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Repository
public interface PaymentDetailsRepository extends JpaRepository<PaymentDetails, Long>, PaymentDetailsCustomRepository {

	/**
	 * Get PaymentDetails by paymentDetails transactionNo and paymentDetails Id not equal if exist
	 *
	 * @param  transactionNo
	 * @param  id
	 * @return
	 */
	Optional<PaymentDetails> findByTransactionNoIgnoreCaseAndIdNot(String transactionNo, Long id);

	/**
	 * Get PaymentDetails by paymentDetails transactionNo if exist
	 *
	 * @param  transactionNo
	 * @return
	 */
	Optional<PaymentDetails> findByTransactionNoIgnoreCase(String transactionNo);

	/**
	 * Get payment details page by paidOn between from date and to date for vendor
	 *
	 * @param fromDate
	 * @param toDate
	 */
	Page<PaymentDetails> findAllByPaidOnBetweenAndVendor(Date fromDate, Date toDate, Vendor vendor, Pageable pageable);

	/**
	 * Get payment details page by paidOn greater than from date for vendor
	 *
	 * @param  fromDate
	 * @return
	 */
	Page<PaymentDetails> findAllByPaidOnGreaterThanEqualAndVendor(Date fromDate, Vendor vendor, Pageable pageable);

	/**
	 * Get payment details page by paidOn less than to date for vendor
	 *
	 * @param  toDate
	 * @return
	 */
	Page<PaymentDetails> findAllByPaidOnLessThanEqualAndVendor(Date toDate, Vendor vendor, Pageable pageable);

	/**
	 * Get payment details page by vendor
	 *
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<PaymentDetails> findAllByVendor(Vendor vendor, Pageable pageable);

	/**
	 * Get payment details page by delivery boy
	 *
	 * @param  deliveryBoy
	 * @param  pageable
	 * @return
	 */
	Page<PaymentDetails> findAllByDeliveryBoy(DeliveryBoy deliveryBoy, Pageable pageable);

	/**
	 * Get payment details page by paidOn less than to date for delivery boy
	 *
	 * @param  toDate
	 * @param  deliveryBoy
	 * @param  pageable
	 * @return
	 */
	Page<PaymentDetails> findAllByPaidOnLessThanEqualAndDeliveryBoy(Date toDate, DeliveryBoy deliveryBoy, Pageable pageable);

	/**
	 * Get payment details page by paidOn greater than from date for delivery boy
	 *
	 * @param  fromDate
	 * @param  deliveryBoy
	 * @param  pageable
	 * @return
	 */
	Page<PaymentDetails> findAllByPaidOnGreaterThanEqualAndDeliveryBoy(Date fromDate, DeliveryBoy deliveryBoy, Pageable pageable);

	/**
	 * Get payment details page by paidOn between from date and to date for delivery boy
	 *
	 * @param  fromDate
	 * @param  toDate
	 * @param  deliveryBoy
	 * @param  pageable
	 * @return
	 */
	Page<PaymentDetails> findAllByPaidOnBetweenAndDeliveryBoy(Date fromDate, Date toDate, DeliveryBoy deliveryBoy, Pageable pageable);

}
