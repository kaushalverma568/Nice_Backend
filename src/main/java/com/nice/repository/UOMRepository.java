package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.UOM;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface UOMRepository extends JpaRepository<UOM, Long> {

	/**
	 * Get UOM Page by active
	 *
	 * @param pageable
	 * @param active
	 * @return
	 */
	Page<UOM> findAllByActive(Boolean active, Pageable pageable);

	/**
	 * Get UOM list by active
	 *
	 * @param active
	 * @return
	 */
	List<UOM> findAllByActive(Boolean active);

	/**
	 *
	 * @param activeRecords
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<UOM> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId, Pageable pageable);

	/**
	 *
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<UOM> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @param vendorId
	 * @return
	 */
	List<UOM> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId);

	/**
	 *
	 * @param vendorId
	 * @return
	 */
	List<UOM> findAllByVendorId(Long vendorId);

	/**
	 * check uom exist based on measurment and qty
	 * @param quantity
	 * @param vendorId TODO
	 * @param measurementEnglish
	 *
	 * @return
	 */
	Optional<UOM> findByMeasurementArabicIgnoreCaseAndQuantityAndVendorId(String measurementArabic, Double quantity, Long vendorId);

	/**
	 * check uom exist based on measurment and qty and id not
	 * @param quantity
	 * @param vendorId
	 * @param id TODO
	 * @param measurementEnglish
	 *
	 * @return
	 */
	Optional<UOM> findByMeasurementArabicIgnoreCaseAndQuantityAndVendorIdAndIdNot(String measurementArabic, Double quantity, Long vendorId, Long id);

	/**
	 * get uom based on measurement english and arabic with qty and vendor
	 *
	 * @param measurementEnglish
	 * @param quantity
	 * @param id
	 * @param measurementArabic
	 * @param quantity2
	 * @param id2
	 * @return
	 */
	Optional<UOM> findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorIdOrMeasurementArabicIgnoreCaseAndQuantityAndVendorId(String measurementEnglish,
			Double quantity, Long id, String measurementArabic, Double quantity2, Long id2);

	/**
	 * @param measurementEnglish
	 * @param quantity
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<UOM> findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorIdAndIdNot(String measurementEnglish, Double quantity, Long vendorId, Long id);

	/**
	 * @param measurementEnglish
	 * @param quantity
	 * @param vendorId
	 * @return
	 */
	Optional<UOM> findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorId(String measurementEnglish, Double quantity, Long vendorId);
}
