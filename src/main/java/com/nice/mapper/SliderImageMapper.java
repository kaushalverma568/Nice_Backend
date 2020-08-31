package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.SliderImageDTO;
import com.nice.dto.SliderImageResponseDTO;
import com.nice.model.SliderImage;
import com.nice.service.AssetService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class SliderImageMapper {

	@Autowired
	private AssetService assetService;

	public SliderImageResponseDTO toDto(final SliderImage sliderBanner) {
		final Locale locale = LocaleContextHolder.getLocale();
		SliderImageResponseDTO sliderImageResponseDTO = new SliderImageResponseDTO();
		BeanUtils.copyProperties(sliderBanner, sliderImageResponseDTO);
		sliderImageResponseDTO.setImageEnglishUrl(assetService.getGeneratedUrl(sliderBanner.getImageNameEnglish(), AssetConstant.SLIDER_IMAGES));
		sliderImageResponseDTO.setImageArabicUrl(assetService.getGeneratedUrl(sliderBanner.getImageNameArabic(), AssetConstant.SLIDER_IMAGES));
		if (locale.getLanguage().equals("en")) {
			sliderImageResponseDTO.setImageUrl(assetService.getGeneratedUrl(sliderBanner.getImageNameEnglish(), AssetConstant.SLIDER_IMAGES));
		} else {
			sliderImageResponseDTO.setImageUrl(assetService.getGeneratedUrl(sliderBanner.getImageNameArabic(), AssetConstant.SLIDER_IMAGES));
		}
		return sliderImageResponseDTO;
	}

	public SliderImage toEntity(final SliderImageDTO sliderBannerDTO) {
		SliderImage sliderBanner = new SliderImage();
		BeanUtils.copyProperties(sliderBannerDTO, sliderBanner);
		return sliderBanner;
	}

	public List<SliderImageResponseDTO> toDtos(final List<SliderImage> images) {
		List<SliderImageResponseDTO> imageDTOs = new ArrayList<>();
		for (SliderImage sliderImage : images) {
			imageDTOs.add(toDto(sliderImage));
		}
		return imageDTOs;
	}

}
