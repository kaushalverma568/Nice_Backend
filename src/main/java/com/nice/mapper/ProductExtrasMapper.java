package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductExtrasDTO;
import com.nice.model.ProductExtras;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Component
public class ProductExtrasMapper {

	public ProductExtrasDTO toDto(final ProductExtras productExtras) {
		ProductExtrasDTO productExtrasResponseDTO = new ProductExtrasDTO();
		BeanUtils.copyProperties(productExtras, productExtrasResponseDTO);
		productExtrasResponseDTO.setProductId(productExtras.getProduct().getId());
		productExtrasResponseDTO.setProductExtrasMasterId(productExtras.getProductExtrasMaster().getId());
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			productExtrasResponseDTO.setName(productExtras.getProductExtrasMaster().getNameEnglish());
			productExtrasResponseDTO.setDescription(productExtras.getProductExtrasMaster().getDescriptionEnglish());
		} else {
			productExtrasResponseDTO.setName(productExtras.getProductExtrasMaster().getNameArabic());
			productExtrasResponseDTO.setDescription(productExtras.getProductExtrasMaster().getDescriptionArabic());
		}
		productExtrasResponseDTO.setRate(productExtras.getRate());
		return productExtrasResponseDTO;
	}

	public ProductExtras toEntity(final ProductExtrasDTO productExtrasDTO) {
		ProductExtras productExtras = new ProductExtras();
		BeanUtils.copyProperties(productExtrasDTO, productExtras);
		return productExtras;
	}

	public List<ProductExtrasDTO> toDtos(final List<ProductExtras> productExtrasList) {
		List<ProductExtrasDTO> results = new ArrayList<>();
		for (ProductExtras ProductExtras : productExtrasList) {
			results.add(toDto(ProductExtras));
		}
		return results;
	}
}
