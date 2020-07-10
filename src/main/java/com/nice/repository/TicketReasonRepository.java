package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.TicketReason;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 09-07-2020
 */
@Repository
public interface TicketReasonRepository extends JpaRepository<TicketReason, Long> {

	/**
	 * find all by type
	 *
	 * @param type
	 */
	List<TicketReason> findAllByType(String type);

	/**
	 * find by reason
	 *
	 * @param reason
	 * @return
	 */
	Optional<TicketReason> findByReason(String reason);
}
