package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductAddonsDTO;
import com.nice.model.ProductAddons;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 02-Jul-2020
 */
@Component
public class ProductAddonsMapper {

	public ProductAddonsDTO toDto(final ProductAddons productAddons) {
		ProductAddonsDTO productAddonsResponseDTO = new ProductAddonsDTO();
		BeanUtils.copyProperties(productAddons, productAddonsResponseDTO);
		productAddonsResponseDTO.setProductVariantId(productAddons.getProductVariant().getId());
		productAddonsResponseDTO.setAddonsId(productAddons.getAddons().getId());
		productAddonsResponseDTO.setAddonsName(productAddons.getAddons().getName());
		return productAddonsResponseDTO;
	}

	public ProductAddons toEntity(final ProductAddonsDTO productAddonsDTO) {
		ProductAddons productAddons = new ProductAddons();
		BeanUtils.copyProperties(productAddonsDTO, productAddons);
		return productAddons;
	}

	public List<ProductAddonsDTO> toDtos(final List<ProductAddons> productAddonsList) {
		List<ProductAddonsDTO> results = new ArrayList<>();
		for (ProductAddons productAddons : productAddonsList) {
			results.add(toDto(productAddons));
		}
		return results;
	}
}
