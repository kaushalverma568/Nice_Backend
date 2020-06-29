package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.BusinessCategory;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface BusinessCategoryRepository extends JpaRepository<BusinessCategory, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<BusinessCategory> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * 
	 * @param name
	 * @param id
	 * @return
	 */

	List<Optional<BusinessCategory>> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * 
	 * @param name
	 * @return
	 */
	List<Optional<BusinessCategory>> findByNameIgnoreCase(String name);

}
