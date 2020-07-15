package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Addons;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 02-Jul-2020
 */
@Repository
public interface ProductAddonsRepository extends JpaRepository<ProductAddons, Long> {

	/**
	 * @param  productVariantId
	 * @param  active
	 * @return
	 */
	List<ProductAddons> findAllByProductVariantIdAndActive(Long productVariantId, Boolean active);

	/**
	 * @param  productVariant
	 * @param  name
	 * @param  id
	 * @return
	 */
	Optional<ProductAddons> findByProductVariantAndAddonsAndIdNot(ProductVariant productVariant, Addons addons, Long id);

	/**
	 * @param  productVariant
	 * @param  name
	 * @return
	 */
	Optional<ProductAddons> findByProductVariantAndAddons(ProductVariant productVariant, Addons addons);

	/**
	 * @param  productVariantId
	 * @return
	 */
	List<ProductAddons> findAllByProductVariantId(Long productVariantId);

	/**
	 * @param  activeRecords
	 * @param  productVariant
	 * @return
	 */
	List<ProductAddons> findAllByProductVariantAndActive(ProductVariant productVariant, Boolean activeRecords);

	/**
	 * @param  productVariant
	 * @return
	 */
	List<ProductAddons> findAllByProductVariant(ProductVariant productVariant);

	/**
	 * @param  addons
	 * @return
	 */
	List<ProductAddons> findAllByAddons(Addons addons);
}
