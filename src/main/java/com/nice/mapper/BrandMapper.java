package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.BrandDTO;
import com.nice.model.Brand;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class BrandMapper {

	public BrandDTO toDto(final Brand brand) {
		Locale locale = LocaleContextHolder.getLocale();
		BrandDTO brandDTO = new BrandDTO();
		BeanUtils.copyProperties(brand, brandDTO);
		if (locale.getLanguage().equals("en")) {
			brandDTO.setName(brand.getNameEnglish());
		} else {
			brandDTO.setName(brand.getNameArabic());
		}
		return brandDTO;
	}

	public Brand toEntity(final BrandDTO brandDTO) {
		Brand brand = new Brand();
		BeanUtils.copyProperties(brandDTO, brand);
		return brand;
	}

	public List<BrandDTO> toDtos(final List<Brand> brands) {
		List<BrandDTO> results = new ArrayList<>();
		for (Brand brand : brands) {
			results.add(toDto(brand));
		}
		return results;
	}
}
