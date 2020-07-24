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
	 * get all toppings by active and vendor id
	 *
	 * @param vendor
	 * @param active
	 * @return
	 */
	List<ProductTopping> findAllByVendorIdAndActive(Long vendorId, Boolean active);

	/**
	 * get all toppings by vendor id
	 *
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 * get topping by name, vendor id and id not
	 *
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Topping> findByNameIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * get topping by name and vendor id
	 *
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<Topping> findByNameIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * get all toppings by active and vendor id
	 *
	 * @param activeRecords
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByActiveAndVendorId(Boolean activeRecords, Long vendorId, Pageable pageable);

	/**
	 * get all toppings by active
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Topping> findAllByActive(Boolean activeRecords, Pageable pageable);

}
