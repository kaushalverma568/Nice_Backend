package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Vendor;

@Repository
public interface VendorRepository extends JpaRepository<Vendor, Long>, VendorCustomRepository {

	/**
	 * Get vendor by vendor email and id not equal if exist
	 *
	 * @param  contactNo
	 * @param  id
	 * @return
	 */
	Optional<Vendor> findByEmailAndIdNot(String email, Long id);

	/**
	 * Get vendor by vendor email if exist
	 *
	 * @param  email
	 * @return
	 */
	Optional<Vendor> findByEmail(String email);

	/**
	 * Get vendor page by active
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Vendor> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get vendor page by active and isEmailVerified
	 *
	 * @param  activeRecords
	 * @param  isEmailVerified
	 * @param  pageable
	 * @return
	 */
	Page<Vendor> findAllByActiveAndIsEmailVerified(Boolean activeRecords, Boolean isEmailVerified, Pageable pageable);

	/**
	 * Get vendor page by isEmailVerified
	 *
	 * @param  isEmailVerified
	 * @param  pageable
	 * @return
	 */
	Page<Vendor> findAllByIsEmailVerified(Boolean isEmailVerified, Pageable pageable);

	/**
	 * Get vendor by vendor email and id not equal if exist
	 *
	 * @param  contactNo
	 * @param  id
	 * @return
	 */
	Optional<Vendor> findByContactNoAndIdNot(String contactNo, Long id);

	/**
	 * Get vendor by vendor email if exist
	 *
	 * @param  email
	 * @return
	 */
	Optional<Vendor> findByContactNo(String contactNo);

	/**
	 * 
	 * @param activeRecords
	 * @return
	 */
	List<Vendor> findAllByActive(Boolean activeRecords);

}
