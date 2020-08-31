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
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 7, 2020
 */
@Repository
public interface TicketRepository extends JpaRepository<Ticket, Long>, TicketCustomRepository {

	/**
	 * get page of ticket by userType and entityId
	 *
	 * @param  entityId
	 * @param  userType
	 * @param  pageable
	 * @return
	 */
	Page<Ticket> findAllByEntityIdAndUserType(Long entityId, String userType, Pageable pageable);

	/**
	 * get page of ticket by userType
	 *
	 * @param  userType
	 * @param  pageable
	 * @return
	 */
	Page<Ticket> findAllByUserType(String userType, Pageable pageable);

	/**
	 * get page of ticket by entityId
	 *
	 * @param  entityId
	 * @param  pageable
	 * @return
	 */
	Page<Ticket> findAllByEntityId(Long entityId, Pageable pageable);
}
