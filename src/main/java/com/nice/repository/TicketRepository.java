/**
 *
 */
package com.nice.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Ticket;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 31-Jan-2020
 */
@Repository
public interface TicketRepository extends JpaRepository<Ticket, Long> {

	/**
	 * get page of ticket by userType and email
	 *
	 * @param email
	 * @param userType
	 * @param pageable
	 * @return
	 */
	Page<Ticket> findAllByEmailAndUserType(String email, String userType, Pageable pageable);

	/**
	 * get page of ticket by userType
	 * 
	 * @param userType
	 * @param pageable
	 * @return
	 */
	Page<Ticket> findAllByUserType(String userType, Pageable pageable);

	/**
	 * get page of ticket by email
	 * 
	 * @param email
	 * @param pageable
	 * @return
	 */
	Page<Ticket> findAllByEmail(String email, Pageable pageable);
}
