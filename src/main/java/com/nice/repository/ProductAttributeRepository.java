package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductAttribute;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductAttributeRepository extends JpaRepository<ProductAttribute, Long> {

	/**
	 *
	 * @param activeRecords
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductAttribute> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<ProductAttribute> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 *
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductAttribute> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 *
	 * @param name
	 * @param id
	 * @return
	 */

	List<Optional<ProductAttribute>> findByNameIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 *
	 * @param name
	 * @return
	 */
	List<Optional<ProductAttribute>> findByNameIgnoreCaseAndVendorId(String name, Long vendorId);

}
