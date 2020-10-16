package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Customer;
import com.nice.model.Orders;
import com.nice.model.WalletTrx;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Repository(value = "walletTrxRepository")
public interface WalletTrxRepository extends JpaRepository<WalletTrx, Long> {

	Page<WalletTrx> findAllByCustomer(Customer customer, Pageable pageable);

	/**
	 * @param order
	 * @param transactionType
	 */
	Optional<WalletTrx> findByOrderAndTransactionType(Orders order, String transactionType);

	/**
	 * @param customerDetails
	 * @param i
	 * @param pageable
	 * @return
	 */
	Page<WalletTrx> findByCustomerAndAmountNot(Customer customerDetails, Double i, Pageable pageable);

	/**
	 * @param i
	 * @param pageable
	 * @return
	 */
	Page<WalletTrx> findAllByAmountNot(Double i, Pageable pageable);

}
