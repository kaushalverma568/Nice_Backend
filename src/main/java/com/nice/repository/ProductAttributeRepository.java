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
	 * @param b
	 * @param vendorId
	 * @return
	 */
	List<ProductAttribute> findAllByActiveAndVendorId(boolean b, Long vendorId);

	/**
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<ProductAttribute> findByNameEnglishIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<ProductAttribute> findByNameEnglishIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<ProductAttribute> findByNameArabicIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<ProductAttribute> findByNameArabicIgnoreCaseAndVendorId(String name, Long vendorId);

}
