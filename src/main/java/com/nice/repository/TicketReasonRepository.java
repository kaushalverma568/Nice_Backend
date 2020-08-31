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
 * @date : 09-07-2020
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
	 * @param reason
	 * @return
	 */
	Optional<List<TicketReason>> findAllByReasonEnglishOrReasonArabic(String reasonEnglish, String reasonArabic);

	/**
	 * get ticket reason ignore case by reason, type ignore case and id not if exist
	 *
	 * @param reasonEnglish
	 * @param type
	 * @param id
	 * @param reasonArabic
	 * @param type2
	 * @param id2
	 * @return
	 */
	Optional<TicketReason> findByReasonEnglishIgnoreCaseAndTypeIgnoreCaseAndIdNotOrReasonArabicIgnoreCaseAndTypeIgnoreCaseAndIdNot(String reasonEnglish,
			String type, Long id, String reasonArabic, String type2, Long id2);

	/**
	 * get ticket reason ignore case by reason and type ignore case if exist
	 *
	 * @param reasonEnglish
	 * @param type
	 * @param reasonArabic
	 * @param type2
	 * @return
	 */
	Optional<TicketReason> findByReasonEnglishIgnoreCaseAndTypeIgnoreCaseOrReasonArabicIgnoreCaseAndTypeIgnoreCase(String reasonEnglish, String type,
			String reasonArabic, String type2);

	/**
	 * @param activeRecords
	 * @param type
	 * @param pageable
	 * @return
	 */
	Page<TicketReason> findAllByActiveAndType(Boolean activeRecords, String type, Pageable pageable);

	/**
	 * @param type
	 * @param pageable
	 * @return
	 */
	Page<TicketReason> findAllByType(String type, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<TicketReason> findAllByActive(Boolean activeRecords, Pageable pageable);
}
