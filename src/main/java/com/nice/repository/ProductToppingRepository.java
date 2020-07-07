/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductTopping;
import com.nice.model.ProductVariant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductToppingRepository extends JpaRepository<ProductTopping, Long> {

	/**
	 * @param productVariantId
	 * @param active
	 * @return
	 */
	List<ProductTopping> findAllByProductVariantIdAndActive(Long productVariantId, Boolean active);

	/**
	 * @param productVariant
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<ProductTopping> findByProductVariantAndNameAndVendorIdAndIdNot(ProductVariant productVariant, String name, Long vendorId, Long id);

	/**
	 * @param productVariant
	 * @param name
	 * @return
	 */
	Optional<ProductTopping> findByProductVariantAndVendorIdAndName(ProductVariant productVariant, Long vendorId, String name);

	/**
	 * @param productVariantId
	 * @return
	 */
	List<ProductTopping> findAllByProductVariantId(Long productVariantId);

}
