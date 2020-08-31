package com.nice.mapper;

import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.CountryDTO;
import com.nice.model.Country;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component
public class CountryMapper {

	public Country toEntity(final CountryDTO countryDTO) {
		Country country = new Country();
		BeanUtils.copyProperties(countryDTO, country);
		return country;
	}

	public CountryDTO toDto(final Country country) {
		Locale locale = LocaleContextHolder.getLocale();
		CountryDTO countryDTO = new CountryDTO();
		BeanUtils.copyProperties(country, countryDTO);
		if (locale.getLanguage().equals("en")) {
			countryDTO.setName(country.getNameEnglish());
		} else {
			countryDTO.setName(country.getNameArabic());
		}
		return countryDTO;
	}
}
