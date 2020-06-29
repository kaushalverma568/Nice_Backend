package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.BusinessCategoryDTO;
import com.nice.model.BusinessCategory;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class BusinessCategoryMapper {
	
	public BusinessCategoryDTO toDto(final BusinessCategory businessCategory) {
		BusinessCategoryDTO businessCategoryResponseDTO = new BusinessCategoryDTO();
		BeanUtils.copyProperties(businessCategory, businessCategoryResponseDTO);
		businessCategoryResponseDTO.setImageUrl(CommonUtility.getGeneratedUrl(businessCategory.getImageName(), AssetConstant.BUSINESS_CATEGORY_DIR));
		return businessCategoryResponseDTO;
	}

	public BusinessCategory toEntity(final BusinessCategoryDTO businessCategoryDTO) {
		BusinessCategory businessCategory = new BusinessCategory();
		BeanUtils.copyProperties(businessCategoryDTO, businessCategory);
		return businessCategory;
	}

	public List<BusinessCategoryDTO> toDtos(final List<BusinessCategory> businessCategoryList) {
		List<BusinessCategoryDTO> results = new ArrayList<>();
		for (BusinessCategory businessCategory : businessCategoryList) {
			results.add(toDto(businessCategory));
		}
		return results;
	}
}
