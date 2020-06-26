package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CityDTO;
import com.nice.dto.CityResponseDTO;
import com.nice.model.City;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Component
public class CityMapper {

	public CityResponseDTO toDto(final City city) {
		CityResponseDTO cityResponseDTO = new CityResponseDTO();
		BeanUtils.copyProperties(city, cityResponseDTO);
		cityResponseDTO.setStateId(city.getState().getId());
		cityResponseDTO.setStateName(city.getState().getName());
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
