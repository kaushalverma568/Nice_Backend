package com.nice.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Customer;
import com.nice.model.WalletTrx;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Repository(value = "walletTrxRepository")
public interface WalletTrxRepository extends JpaRepository<WalletTrx, Long> {

	Page<WalletTrx> findAllByCustomer(Customer customer, Pageable pageable);

}
