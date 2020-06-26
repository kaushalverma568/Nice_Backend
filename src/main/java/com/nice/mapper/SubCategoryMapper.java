/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.SubCategoryDTO;
import com.nice.dto.SubCategoryResponseDTO;
import com.nice.model.SubCategory;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class SubCategoryMapper {

	public SubCategoryResponseDTO toDto(final SubCategory subCategory) {
		SubCategoryResponseDTO subCategoryResponseDTO = new SubCategoryResponseDTO();
		BeanUtils.copyProperties(subCategory, subCategoryResponseDTO);
		subCategoryResponseDTO.setCategoryId(subCategory.getCategory().getId());
		subCategoryResponseDTO.setCategoryName(subCategory.getCategory().getName());
		if (subCategory.getImage() != null) {
			subCategoryResponseDTO.setImage(CommonUtility.getGeneratedUrl(subCategory.getImage(), AssetConstant.SUB_CATEGORY));
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
