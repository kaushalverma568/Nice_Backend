package com.nice.repository;

import java.util.List;
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
	Optional<ProductExtrasMaster> findByNameIgnoreCaseAndIdNot(String name, Long id);

	
	/**
	 *
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	List<ProductExtrasMaster> findByNameIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 *
	 * @param name
	 * @return
	 */
	Optional<ProductExtrasMaster> findByNameIgnoreCase(String name);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @return
	 */
	List<ProductExtrasMaster> findByNameIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductExtrasMaster> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 * 
	 * @param activeRecords
	 * @return
	 */
	List<ProductExtrasMaster> findAllByActive(Boolean activeRecords);


	/**
	 * 
	 * @param activeRecords
	 * @param vendorId
	 * @return
	 */
	List<ProductExtrasMaster> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId);


	/**
	 * 
	 * @param vendorId
	 * @return
	 */
	List<ProductExtrasMaster> findAllByVendorId(Long vendorId);



}
