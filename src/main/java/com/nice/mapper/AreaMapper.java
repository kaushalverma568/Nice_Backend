package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.AreaDTO;
import com.nice.dto.AreaResponseDTO;
import com.nice.model.Area;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Component
public class AreaMapper {

	public Area toEntity(final AreaDTO areaDTO) {
		Area area = new Area();
		BeanUtils.copyProperties(areaDTO, area);
		return area;
	}

	public AreaResponseDTO toDto(final Area area) {
		Locale locale = LocaleContextHolder.getLocale();
		AreaResponseDTO areaDTO = new AreaResponseDTO();
		BeanUtils.copyProperties(area, areaDTO);
		areaDTO.setCityId(area.getCity().getId());
		areaDTO.setCityNameArabic(area.getCity().getNameArabic());
		areaDTO.setCityNameEnglish(area.getCity().getNameEnglish());
		if (locale.getLanguage().equals("en")) {
			areaDTO.setName(area.getNameEnglish());
			areaDTO.setCityName(area.getCity().getNameEnglish());
		} else {
			areaDTO.setCityName(area.getCity().getNameArabic());
			areaDTO.setName(area.getNameArabic());
		}
		return areaDTO;
	}

	public List<AreaResponseDTO> toDtos(final List<Area> areas) {
		List<AreaResponseDTO> areaDTOs = new ArrayList<>();
		for (Area area : areas) {
			areaDTOs.add(toDto(area));
		}
		return areaDTOs;
	}
}
