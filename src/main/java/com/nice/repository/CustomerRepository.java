package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.Customer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
@Repository
public interface CustomerRepository extends JpaRepository<Customer, Long> {

	/**
	 * Get Customers by Customers email if exist
	 *
	 * @param email
	 * @return
	 */
	Optional<Customer> findByEmail(String email);

	/**
	 * Get Customers by Customers email and Customers Id not equal if exist
	 *
	 * @param email
	 * @param id
	 * @return
	 */

	Optional<Customer> findByEmailAndIdNot(String email, Long id);

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Customer> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param active
	 * @return
	 */
	long countByActive(boolean active);

	/**
	 * get list of customers by active
	 *
	 * @param activeRecords
	 * @return
	 */
	List<Customer> findAllByActive(Boolean activeRecords);

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param pageable
	 * @return
	 */
	Page<Customer> findAllByActiveAndFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCaseOrPhoneNumberContainingIgnoreCase(Boolean activeRecords,
			String searchKeyword, String searchKeyword2, String searchKeyword3, Pageable pageable);

	/**
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param pageable
	 * @return
	 */
	Page<Customer> findAllByFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCaseOrPhoneNumberContainingIgnoreCase(String searchKeyword,
			String searchKeyword2, String searchKeyword3, Pageable pageable);

	/**
	 * @param phoneNumber
	 * @param customerId
	 * @return
	 */
	Optional<Customer> findByPhoneNumberIgnoreCaseAndIdNot(String phoneNumber, Long customerId);

	/**
	 * @param phoneNumber
	 * @return
	 */
	Optional<Customer> findByPhoneNumberIgnoreCase(String phoneNumber);

	/**
	 * @param entityId
	 */
	@Query(value = "Select cust.walletAmt from Customer cust where cust.id = :id")
	Double getWalletAmountForCustomer(Long id);

}