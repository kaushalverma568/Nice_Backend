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
 * @date : 14-Jul-2020
 */
@Repository
public interface AddonsRepository extends JpaRepository<Addons, Long> {

	/**
	 * get Addons page name containing search keyword
	 *
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByNameEnglishContainingIgnoreCase(String searchKeyword, Pageable pageable);

	/**
	 * get Addons page name containing search keyword andactive
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameEnglishContainingIgnoreCase(Boolean activeRecords, String searchKeyword, Pageable pageable);

	/**
	 * get Addons page name containing search keyword andactive
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyword, Pageable pageable);

	/**
	 * get Addons list by vendor
	 *
	 * @param vendor
	 * @return
	 */
	List<Addons> findAllByVendor(Vendor vendor);

	/**
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByVendor(Vendor vendor, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndVendor(Boolean activeRecords, Vendor vendor, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * @param nameEnglish
	 * @param vendor
	 * @param id
	 * @return
	 */
	Optional<Addons> findByNameEnglishIgnoreCaseAndVendorAndIdNot(String nameEnglish, Vendor vendor, Long id);

	/**
	 * @param nameEnglish
	 * @param vendor
	 * @return
	 */
	Optional<Addons> findByNameEnglishIgnoreCaseAndVendor(String nameEnglish, Vendor vendor);

	/**
	 * @param nameArabic
	 * @param vendor
	 * @param id
	 * @return
	 */
	Optional<Addons> findByNameArabicIgnoreCaseAndVendorAndIdNot(String nameArabic, Vendor vendor, Long id);

	/**
	 * @param nameArabic
	 * @param vendor
	 * @return
	 */
	Optional<Addons> findByNameArabicIgnoreCaseAndVendor(String nameArabic, Vendor vendor);

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameEnglishContainingIgnoreCaseAndVendor(Boolean activeRecords, String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByActiveAndNameArabicContainingIgnoreCaseAndVendor(Boolean activeRecords, String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByNameEnglishContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param searchKeyword
	 * @param vendor
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByNameArabicContainingIgnoreCaseAndVendor(String searchKeyword, Vendor vendor, Pageable pageable);

	/**
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	Page<Addons> findAllByNameArabicContainingIgnoreCase(String searchKeyword, Pageable pageable);
}
