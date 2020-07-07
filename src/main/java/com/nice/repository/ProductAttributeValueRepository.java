package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort.Order;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.util.Streamable;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductAttribute;
import com.nice.model.ProductAttributeValue;
import com.nice.model.ProductVariant;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductAttributeValueRepository extends JpaRepository<ProductAttributeValue, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<ProductAttributeValue> findAllByActive(Boolean activeRecords, Pageable pageable);

	List<Optional<ProductAttributeValue>> findByProductAttributeAndIdNot(ProductAttribute productAttribute, Long id);

	Streamable<Order> findByProductAttribute(ProductAttribute productAttribute);

	Optional<ProductAttribute> findByProductVariantAndProductAttributeAndAttributeValueAndIdNot(ProductVariant productVariant,
			ProductAttribute productAttribute, String value, Long id);

	Optional<ProductAttribute> findByProductVariantAndProductAttributeAndAttributeValue(ProductVariant productVariant, ProductAttribute productAttribute,
			String value);

	/**
	 * @param productVariant
	 * @param activeRecords
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductVariantAndActive(ProductVariant productVariant, Boolean activeRecords);

	/**
	 * @param productVariant
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductVariant(ProductVariant productVariant);
}
