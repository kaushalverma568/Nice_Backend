package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.dto.BusinessCategoryDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.BusinessCategory;
import com.nice.service.AssetService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Component
public class BusinessCategoryMapper {

	@Autowired
	private AssetService assetService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	public BusinessCategoryDTO toDto(final BusinessCategory businessCategory) {
		final Locale locale = LocaleContextHolder.getLocale();
		BusinessCategoryDTO businessCategoryResponseDTO = new BusinessCategoryDTO();
		BeanUtils.copyProperties(businessCategory, businessCategoryResponseDTO);
		if (locale.getLanguage().equals("en")) {
			businessCategoryResponseDTO.setName(businessCategory.getNameEnglish());
		} else {
			businessCategoryResponseDTO.setName(businessCategory.getNameArabic());
		}
		businessCategoryResponseDTO.setIsDefault(Constant.BUSINESS_CATEGORY_FOOD_ENGLISH.equals(businessCategoryResponseDTO.getNameEnglish()));
		businessCategoryResponseDTO.setImageUrl(assetService.getGeneratedUrl(businessCategory.getImageName(), AssetConstant.BUSINESS_CATEGORY_DIR));
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
