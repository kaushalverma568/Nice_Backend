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
	 * Get UOM by uom measurement and uom Id not equal if exist
	 *
	 * @param measurement
	 * @param id
	 * @return
	 */
	Optional<UOM> findByMeasurementIgnoreCaseAndQuantityAndIdNot(String measurement, Double quantity, Long id);

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
	 * Get UOM by uom measurement if exist
	 *
	 * @param measurement
	 * @param classification
	 * @return
	 */
	Optional<UOM> findByMeasurementIgnoreCaseAndQuantity(String measurement, Double quantity);

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
	 * @param measurement
	 * @param quantity
	 * @param id
	 * @return
	 */
	Optional<UOM> findByMeasurementIgnoreCaseAndQuantityAndVendorId(String measurement, Double quantity, Long id);

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
}
