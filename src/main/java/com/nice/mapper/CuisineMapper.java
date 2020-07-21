package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.CuisineDTO;
import com.nice.dto.CuisineResponseDTO;
import com.nice.model.Cuisine;
import com.nice.service.AssetService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Component
public class CuisineMapper {

	@Autowired
	private AssetService assetService;

	public CuisineResponseDTO toDto(final Cuisine cuisine) {
		CuisineResponseDTO cuisineResponseDTO = new CuisineResponseDTO();
		BeanUtils.copyProperties(cuisine, cuisineResponseDTO);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(cuisine.getImageName())) {
			cuisineResponseDTO.setImageUrl(assetService.getGeneratedUrl(cuisine.getImageName(), AssetConstant.CUISINE));
		}
		return cuisineResponseDTO;
	}

	public Cuisine toEntity(final CuisineDTO cuisineDTO) {
		Cuisine cuisine = new Cuisine();
		BeanUtils.copyProperties(cuisineDTO, cuisine);
		return cuisine;
	}

	public List<CuisineResponseDTO> toDtos(final List<Cuisine> cuisines) {
		List<CuisineResponseDTO> results = new ArrayList<>();
		for (Cuisine cuisine : cuisines) {
			results.add(toDto(cuisine));
		}
		return results;
	}
}
