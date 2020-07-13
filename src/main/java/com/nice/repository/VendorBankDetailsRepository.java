package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 01-Jul-2020
 */
@Repository
public interface VendorBankDetailsRepository extends JpaRepository<VendorBankDetails, Long> {

	/**
	 * get vendor bank details
	 *
	 * @param  vendor
	 * @return
	 */
	Optional<VendorBankDetails> findByVendor(Vendor vendor);

}
