package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CountryDTO;
import com.nice.model.Country;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Component
public class CountryMapper {

	public CountryDTO toDto(final Country country) {
		CountryDTO countryDTO = new CountryDTO();
		BeanUtils.copyProperties(country, countryDTO);
		return countryDTO;
	}

	public Country toEntity(final CountryDTO countryDTO) {
		Country country = new Country();
		BeanUtils.copyProperties(countryDTO, country);
		return country;
	}

	public List<CountryDTO> toDtos(final List<Country> countries) {
		List<CountryDTO> results = new ArrayList<>();
		for (Country c : countries) {
			results.add(toDto(c));
		}
		return results;
	}
}
