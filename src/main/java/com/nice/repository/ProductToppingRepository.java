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
import com.nice.model.Topping;

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
	 *
	 * @param topping
	 * @param active
	 * @return
	 */
	List<ProductTopping> findAllByToppingAndActive(Topping topping, Boolean active);

	/**
	 * @param productVariantId
	 * @return
	 */
	List<ProductTopping> findAllByProductVariantId(Long productVariantId);

	/**
	 * @param topping
	 * @return
	 */
	List<ProductTopping> findAllByTopping(Topping topping);

	/**
	 * @param productVariant
	 * @param topping
	 * @return
	 */
	Optional<ProductTopping> findByProductVariantAndTopping(ProductVariant productVariant, Topping topping);

	/**
	 * @param productVariant
	 * @param topping
	 * @param id
	 * @return
	 */
	Optional<ProductTopping> findByProductVariantAndToppingAndIdNot(ProductVariant productVariant, Topping topping, Long id);

}
