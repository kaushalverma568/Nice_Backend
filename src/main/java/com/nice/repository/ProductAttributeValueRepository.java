package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
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

	List<Optional<ProductAttributeValue>> findByProductAttributeAndIdNot(ProductAttribute productAttribute, Long id);

	/**
	 * @param productVariant
	 * @param activeRecords
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductVariantAndActiveOrderByRateAsc(ProductVariant productVariant, Boolean activeRecords);

	/**
	 * @param productVariant
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductVariantOrderByRateAsc(ProductVariant productVariant);

	/**
	 * get attribute value list by product attribute and active
	 *
	 * @param productAttribute
	 * @param activeRecords
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductAttributeAndActive(ProductAttribute productAttribute, Boolean activeRecords);

	/**
	 * get attribute value list by product attribute
	 *
	 * @param productAttribute
	 * @return
	 */
	List<ProductAttributeValue> findAllByProductAttribute(ProductAttribute productAttribute);

	/**
	 * get attribute value list by active
	 *
	 * @param activeRecords
	 * @return
	 */
	List<ProductAttributeValue> findAllByActive(Boolean activeRecords);

	/**
	 * @param productVariant
	 * @param productAttribute
	 * @param attributeValueEnglish
	 * @param id
	 * @return
	 */
	Optional<ProductAttributeValue> findByProductVariantAndProductAttributeAndAttributeValueEnglishAndIdNot(ProductVariant productVariant,
			ProductAttribute productAttribute, String attributeValueEnglish, Long id);

	/**
	 * @param productVariant
	 * @param productAttribute
	 * @param attributeValueArabic
	 * @param id
	 * @return
	 */
	Optional<ProductAttributeValue> findByProductVariantAndProductAttributeAndAttributeValueArabicAndIdNot(ProductVariant productVariant,
			ProductAttribute productAttribute, String attributeValueArabic, Long id);

	/**
	 * @param productVariant
	 * @param productAttribute
	 * @param attributeValueEnglish
	 * @return
	 */
	Optional<ProductAttributeValue> findByProductVariantAndProductAttributeAndAttributeValueEnglish(ProductVariant productVariant,
			ProductAttribute productAttribute, String attributeValueEnglish);

	/**
	 * @param productVariant
	 * @param productAttribute
	 * @param attributeValueArabic
	 * @return
	 */
	Optional<ProductAttributeValue> findByProductVariantAndProductAttributeAndAttributeValueArabic(ProductVariant productVariant,
			ProductAttribute productAttribute, String attributeValueArabic);
}
