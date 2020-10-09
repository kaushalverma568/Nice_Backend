package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductExtrasMaster;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductExtrasMasterRepository extends JpaRepository<ProductExtrasMaster, Long> {

	/**
	 *
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameEnglishIgnoreCaseAndIdNot(String name, Long id);

	/**
	 *
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameArabicIgnoreCaseAndIdNot(String name, Long id);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameEnglishIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameArabicIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 *
	 * @param name
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameEnglishIgnoreCase(String name);

	/**
	 *
	 * @param name
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameArabicIgnoreCase(String name);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameEnglishIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameArabicIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductExtrasMaster> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductExtrasMaster> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<ProductExtrasMaster> findAllByActive(Boolean activeRecords, Pageable pageable);

}
