package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductAttributeValueDTO;
import com.nice.model.ProductAttributeValue;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Component
public class ProductAttributeValueMapper {

	public ProductAttributeValueDTO toDto(final ProductAttributeValue productAttributeValue) {
		ProductAttributeValueDTO productAttributeValueResponseDTO = new ProductAttributeValueDTO();
		BeanUtils.copyProperties(productAttributeValue, productAttributeValueResponseDTO);
		productAttributeValueResponseDTO.setProductVariantId(productAttributeValue.getProductVariant().getId());
		productAttributeValueResponseDTO.setProductAttributeId(productAttributeValue.getProductAttribute().getId());
		productAttributeValueResponseDTO.setProductAttributeName(productAttributeValue.getProductAttribute().getName());
		return productAttributeValueResponseDTO;
	}

	public ProductAttributeValue toEntity(final ProductAttributeValueDTO productAttributeValueDTO) {
		ProductAttributeValue productAttributeValue = new ProductAttributeValue();
		BeanUtils.copyProperties(productAttributeValueDTO, productAttributeValue);
		return productAttributeValue;
	}

	public List<ProductAttributeValueDTO> toDtos(final List<ProductAttributeValue> productAttributeValueList) {
		List<ProductAttributeValueDTO> results = new ArrayList<>();
		for (ProductAttributeValue ProductAttributeValue : productAttributeValueList) {
			results.add(toDto(ProductAttributeValue));
		}
		return results;
	}
}