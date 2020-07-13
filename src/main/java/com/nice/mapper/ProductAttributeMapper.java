package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductAttributeDTO;
import com.nice.model.ProductAttribute;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Component
public class ProductAttributeMapper {

	public ProductAttributeDTO toDto(final ProductAttribute productAttribute) {
		ProductAttributeDTO productAttributeResponseDTO = new ProductAttributeDTO();
		BeanUtils.copyProperties(productAttribute, productAttributeResponseDTO);
		return productAttributeResponseDTO;
	}

	public ProductAttribute toEntity(final ProductAttributeDTO productAttributeDTO) {
		ProductAttribute productAttribute = new ProductAttribute();
		BeanUtils.copyProperties(productAttributeDTO, productAttribute);
		return productAttribute;
	}

	public List<ProductAttributeDTO> toDtos(final List<ProductAttribute> productAttributeList) {
		List<ProductAttributeDTO> results = new ArrayList<>();
		for (ProductAttribute productAttribute : productAttributeList) {
			results.add(toDto(productAttribute));
		}
		return results;
	}
}
