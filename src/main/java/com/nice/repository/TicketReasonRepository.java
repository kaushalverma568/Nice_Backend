package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.TicketReason;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 09-07-2020
 */
@Repository
public interface TicketReasonRepository extends JpaRepository<TicketReason, Long> {

	/**
	 * get all ticket reason by type
	 *
	 * @param type
	 */
	List<TicketReason> findAllByType(String type);

	/**
	 * get all ticket reason by reason if exist
	 *
	 * @param  reason
	 * @return
	 */
	Optional<List<TicketReason>> findAllByReason(String reason);

	/**
	 * get ticket reason ignore case by reason, type ignore case and id not if exist
	 *
	 * @param  reason
	 * @param  type
	 * @param  id
	 * @return
	 */
	Optional<TicketReason> findByReasonIgnoreCaseAndTypeIgnoreCaseAndIdNot(String reason, String type, Long id);

	/**
	 * get ticket reason ignore case by reason and type ignore case if exist
	 *
	 * @param  reason
	 * @param  type
	 * @return
	 */
	Optional<TicketReason> findByReasonIgnoreCaseAndTypeIgnoreCase(String reason, String type);

	/**
	 * @param  activeRecords
	 * @param  type
	 * @param  pageable
	 * @return
	 */
	Page<TicketReason> findAllByActiveAndType(Boolean activeRecords, String type, Pageable pageable);

	/**
	 * @param  type
	 * @param  pageable
	 * @return
	 */
	Page<TicketReason> findAllByType(String type, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<TicketReason> findAllByActive(Boolean activeRecords, Pageable pageable);
}
