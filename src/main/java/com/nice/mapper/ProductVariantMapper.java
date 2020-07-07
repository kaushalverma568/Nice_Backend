package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductVariantRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.model.ProductVariant;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component(value = "productVariantMapper")
public class ProductVariantMapper {
	public ProductVariantRequestDTO toRequestDto(final ProductVariant productVariant) {
		ProductVariantRequestDTO productVariantRequestDTO = new ProductVariantRequestDTO();
		BeanUtils.copyProperties(productVariant, productVariantRequestDTO);
		return productVariantRequestDTO;
	}

	public ProductVariantResponseDTO toResponseDto(final ProductVariant productVariant) {
		ProductVariantResponseDTO productVariantResponseDTO = new ProductVariantResponseDTO();
		BeanUtils.copyProperties(productVariant, productVariantResponseDTO);
		return productVariantResponseDTO;
	}

	public ProductVariant toEntity(final ProductVariantRequestDTO productVariantRequestDTO) {
		ProductVariant productVariant = new ProductVariant();
		BeanUtils.copyProperties(productVariantRequestDTO, productVariant);
		return productVariant;
	}

	public ProductVariant toEntityFromResponseDto(final ProductVariantResponseDTO productVariantResponseDTO, final Long userId) {
		ProductVariant productVariant = new ProductVariant();
		BeanUtils.copyProperties(productVariantResponseDTO, productVariant);
		if (productVariantResponseDTO.getId() == null) {
			productVariant.setCreatedBy(userId);
		}
		productVariant.setUpdatedBy(userId);
		return productVariant;
	}

	public List<ProductVariantResponseDTO> toResponseDtos(final List<ProductVariant> productVariants) {
		List<ProductVariantResponseDTO> results = new ArrayList<>();
		for (ProductVariant p : productVariants) {
			results.add(toResponseDto(p));
		}
		return results;
	}
}
