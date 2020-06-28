/**
 *
 */
package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.HtmlSection;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Repository
public interface HtmlSectionRepository extends JpaRepository<HtmlSection, Long> {

	/**
	 * To find by section value and ignore case
	 *
	 * @param sectionValue
	 * @return
	 */
	Optional<HtmlSection> findBySectionTypeIgnoreCase(String sectionType);

	/**
	 * To find by section value and ignore case and id not
	 *
	 * @param sectionValue
	 * @param sectionType
	 * @param id
	 * @return
	 */
	Optional<HtmlSection> findBySectionTypeIgnoreCaseAndIdNot(String sectionType, Long id);

	Optional<HtmlSection> findBySectionType(String type);

}