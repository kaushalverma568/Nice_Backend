package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.CategoryDTO;
import com.nice.dto.CategoryResponseDTO;
import com.nice.model.Category;
import com.nice.service.AssetService;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class CategoryMapper {

	@Autowired
	private AssetService assetService;

	public CategoryResponseDTO toDto(final Category category) {
		final Locale locale = LocaleContextHolder.getLocale();
		CategoryResponseDTO categoryResponseDTO = new CategoryResponseDTO();
		BeanUtils.copyProperties(category, categoryResponseDTO);
		if (locale.getLanguage().equals("en")) {
			categoryResponseDTO.setName(category.getNameEnglish());
		} else {
			categoryResponseDTO.setName(category.getNameArabic());
		}
		if (category.getImage() != null) {
			categoryResponseDTO.setImage(assetService.getGeneratedUrl(category.getImage(), AssetConstant.CATEGORY));
		}
		return categoryResponseDTO;
	}

	public Category toEntity(final CategoryDTO categoryDTO) {
		Category category = new Category();
		BeanUtils.copyProperties(categoryDTO, category);
		return category;
	}

	public List<CategoryResponseDTO> toDtos(final List<Category> categories) {
		List<CategoryResponseDTO> results = new ArrayList<>();
		for (Category category : categories) {
			results.add(toDto(category));
		}
		return results;
	}

}
