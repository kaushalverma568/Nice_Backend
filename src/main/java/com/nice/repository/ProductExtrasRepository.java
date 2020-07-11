package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Product;
import com.nice.model.ProductExtras;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductExtrasRepository extends JpaRepository<ProductExtras, Long> {

	/**
	 *
	 * @param product
	 * @param active
	 * @return
	 */
	List<ProductExtras> findAllByProductAndActive(Product product, Boolean active);

	/**
	 *
	 * @param product
	 * @return
	 */
	List<ProductExtras> findAllByProduct(Product product);

	/**
	 *
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<ProductExtras> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 *
	 * @param name
	 * @param product
	 * @param id
	 * @return
	 */
	Optional<ProductExtras> findByNameIgnoreCaseAndProductAndIdNot(String name, Product product, Long id);

	/**
	 *
	 * @param name
	 * @param product
	 * @return
	 */
	Optional<ProductExtras> findByNameIgnoreCaseAndProduct(String name, Product product);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	List<ProductExtras> findByNameIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 *
	 * @param name
	 * @return
	 */
	List<ProductExtras> findByNameIgnoreCase(String name);

	/**
	 *
	 * @param name
	 * @param vendorId
	 * @return
	 */
	List<ProductExtras> findByNameIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductExtras> findAllByVendorId(Long vendorId, Pageable pageable);

}
