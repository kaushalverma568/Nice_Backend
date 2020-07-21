package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.SliderImageDTO;
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

	public SliderImageDTO toDto(final SliderImage sliderBanner) {
		SliderImageDTO sliderBannerDTO = new SliderImageDTO();
		BeanUtils.copyProperties(sliderBanner, sliderBannerDTO);
		sliderBannerDTO.setImageUrl(assetService.getGeneratedUrl(sliderBanner.getImageName(), AssetConstant.SLIDER_IMAGES));
		return sliderBannerDTO;
	}

	public SliderImage toEntity(final SliderImageDTO sliderBannerDTO) {
		SliderImage sliderBanner = new SliderImage();
		BeanUtils.copyProperties(sliderBannerDTO, sliderBanner);
		return sliderBanner;
	}

	public List<SliderImageDTO> toDtos(final List<SliderImage> images) {
		List<SliderImageDTO> imageDTOs = new ArrayList<>();
		for (SliderImage sliderImage : images) {
			imageDTOs.add(toDto(sliderImage));
		}
		return imageDTOs;
	}

}
