package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.model.Product;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component(value = "prouctMapper")
public class ProductMapper {
	public ProductRequestDTO toRequestDto(final Product product) {
		ProductRequestDTO productRequestDTO = new ProductRequestDTO();
		BeanUtils.copyProperties(product, productRequestDTO);
		return productRequestDTO;
	}

	public ProductResponseDTO toResponseDto(final Product product) {
		ProductResponseDTO productResponseDTO = new ProductResponseDTO();
		if (LocaleContextHolder.getLocale().getLanguage().equals("fr")) {
			productResponseDTO.setName(product.getNameArabic());
			productResponseDTO.setDescription(product.getDescriptionArabic());
		} else {
			productResponseDTO.setName(product.getNameEnglish());
			productResponseDTO.setDescription(product.getDescriptionEnglish());
		}

		BeanUtils.copyProperties(product, productResponseDTO);
		return productResponseDTO;
	}

	public Product toEntity(final ProductRequestDTO productRequestDTO) {
		Product product = new Product();
		BeanUtils.copyProperties(productRequestDTO, product);
		return product;
	}

	public List<ProductResponseDTO> toResponseDtos(final List<Product> products) {
		List<ProductResponseDTO> results = new ArrayList<>();
		for (Product p : products) {
			results.add(toResponseDto(p));
		}
		return results;
	}
}
