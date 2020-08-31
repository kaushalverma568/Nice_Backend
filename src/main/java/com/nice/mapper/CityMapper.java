package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.CityDTO;
import com.nice.dto.CityResponseDTO;
import com.nice.model.City;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component
public class CityMapper {
	public CityResponseDTO toDto(final City city) {
		final Locale locale = LocaleContextHolder.getLocale();
		CityResponseDTO cityResponseDTO = new CityResponseDTO();
		BeanUtils.copyProperties(city, cityResponseDTO);
		if (locale.getLanguage().equals("en")) {
			cityResponseDTO.setName(city.getNameEnglish());
			cityResponseDTO.setStateName(city.getState().getNameEnglish());
		} else {
			cityResponseDTO.setName(city.getNameArabic());
			cityResponseDTO.setStateName(city.getState().getNameArabic());
		}
		cityResponseDTO.setStateId(city.getState().getId());
		return cityResponseDTO;
	}

	public City toEntity(final CityDTO cityDTO) {
		City city = new City();
		BeanUtils.copyProperties(cityDTO, city);
		return city;
	}

	public List<CityResponseDTO> toDtos(final List<City> cityList) {
		List<CityResponseDTO> results = new ArrayList<>();
		for (City city : cityList) {
			results.add(toDto(city));
		}
		return results;
	}
}
