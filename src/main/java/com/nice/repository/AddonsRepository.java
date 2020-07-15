package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Addons;
import com.nice.model.Vendor;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 14-Jul-2020
 */
@Repository
public interface AddonsRepository extends JpaRepository<Addons, Long> {

	/**
	 * Get Addons by name and vendor Id and Id not equal if exist
	 *
	 * @param  name
	 * @param  vendorId
	 * @param  id
	 * @return
	 */
	Optional<Addons> findByNameIgnoreCaseAndVendorAndIdNot(String name, Vendor vendor, Long id);

	/**
	 * Get Addons by name and vendor Id if exist
	 *
	 * @param  name
	 * @param  vendorId
	 * @return
	 */
	Optional<Addons> findByNameIgnoreCaseAndVendor(String name, Vendor vendor);

	/**
	 * get Addons page name containing search keyword
	 *
	 * @param  searchKeyword
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByNameContainingIgnoreCase(String searchKeyword, Pageable pageable);

	/**
	 * get Addons page name containing search keyword andactive
	 *
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword, Pageable pageable);

	/**
	 * get Addons list by vendor
	 *
	 * @param  vendor
	 * @return
	 */
	List<Addons> findAllByVendor(Vendor vendor);

	/**
	 * @param  searchKeyword
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByNameContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByVendor(Vendor vendor, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameContainingIgnoreCaseAndVendor(Boolean activeRecords, String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  vendor
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Addons> findAllByActive(Boolean activeRecords, Pageable pageable);
}
