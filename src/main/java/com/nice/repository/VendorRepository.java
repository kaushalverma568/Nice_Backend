package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Vendor;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface VendorRepository extends JpaRepository<Vendor, Long>, VendorCustomRepository {

	/**
	 * Get vendor by vendor email and id not equal if exist
	 *
	 * @param  email
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
	 * @param  emailVerified
	 * @param  pageable
	 * @return
	 */
	Page<Vendor> findAllByActiveAndEmailVerified(Boolean activeRecords, Boolean emailVerified, Pageable pageable);

	/**
	 * Get vendor page by isEmailVerified
	 *
	 * @param  emailVerified
	 * @param  pageable
	 * @return
	 */
	Page<Vendor> findAllByEmailVerified(Boolean emailVerified, Pageable pageable);

	/**
	 * Get vendor by vendor phonenumber and id not equal if exist
	 *
	 * @param  phoneNumber
	 * @param  id
	 * @return
	 */
	Optional<Vendor> findByPhoneNumberAndIdNot(String phoneNumber, Long id);

	/**
	 * Get vendor by vendor email if exist
	 *
	 * @param  email
	 * @return
	 */
	Optional<Vendor> findByPhoneNumber(String phoneNumber);

	/**
	 * @param  activeRecords
	 * @return
	 */
	List<Vendor> findAllByActive(Boolean activeRecords);

}
