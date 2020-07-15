/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductTopping;
import com.nice.model.Topping;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 14-07-2020
 */
@Repository
public interface ToppingRepository extends JpaRepository<Topping, Long> {

	/**
	 *
	 * @param vendor
	 * @param active
	 * @return
	 */
	List<ProductTopping> findAllByVendorIdAndActive(Long vendorId, Boolean active);

	/**
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Topping> findByNameIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<Topping> findByNameIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param activeRecords
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId, Pageable pageable);

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByActiveAndNameContainingIgnoreCaseAndVendorId(Boolean activeRecords, String searchKeyword, Long vendorId, Pageable pageable);

	/**
	 * @param searchKeyword
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByNameContainingIgnoreCaseAndVendorId(String searchKeyword, Long vendorId, Pageable pageable);

}
