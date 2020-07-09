package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.VendorCuisine;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 01-Jul-2020
 */
@Repository
public interface VendorCuisineRepository extends JpaRepository<VendorCuisine, Long> {

	Optional<VendorCuisine> findByVendorIdAndCuisineIdAndIdNot(Long vendorId, Long cuisineId, Long id);

	Optional<VendorCuisine> findByVendorIdAndCuisineId(Long vendorId, Long cuisineId);

	List<VendorCuisine> findAllByVendorId(Long vendorId);

	List<VendorCuisine> findAllByVendorIdAndActive(Long vendorId, Boolean active);
}
