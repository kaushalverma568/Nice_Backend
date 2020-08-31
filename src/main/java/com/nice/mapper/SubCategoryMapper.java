/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.SubCategoryDTO;
import com.nice.dto.SubCategoryResponseDTO;
import com.nice.model.SubCategory;
import com.nice.service.AssetService;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class SubCategoryMapper {

	@Autowired
	private AssetService assetService;

	public SubCategoryResponseDTO toDto(final SubCategory subCategory) {
		final Locale locale = LocaleContextHolder.getLocale();
		SubCategoryResponseDTO subCategoryResponseDTO = new SubCategoryResponseDTO();
		BeanUtils.copyProperties(subCategory, subCategoryResponseDTO);
		subCategoryResponseDTO.setCategoryId(subCategory.getCategory().getId());
		if (locale.getLanguage().equals("en")) {
			subCategoryResponseDTO.setName(subCategory.getNameEnglish());
			subCategoryResponseDTO.setCategoryName(subCategory.getCategory().getNameEnglish());
		} else {
			subCategoryResponseDTO.setName(subCategory.getNameArabic());
			subCategoryResponseDTO.setCategoryName(subCategory.getCategory().getNameArabic());
		}
		if (subCategory.getImage() != null) {
			subCategoryResponseDTO.setImage(assetService.getGeneratedUrl(subCategory.getImage(), AssetConstant.SUB_CATEGORY));
		}
		return subCategoryResponseDTO;
	}

	public SubCategory toEntity(final SubCategoryDTO subCategoryDTO) {
		SubCategory subCategory = new SubCategory();
		BeanUtils.copyProperties(subCategoryDTO, subCategory);
		return subCategory;
	}

	public List<SubCategoryResponseDTO> toDtos(final List<SubCategory> subCategories) {
		List<SubCategoryResponseDTO> results = new ArrayList<>();
		for (SubCategory subCategory : subCategories) {
			results.add(toDto(subCategory));
		}
		return results;
	}
}
