/**
 *
 */
package com.nice.mapper;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.HtmlSectionDTO;
import com.nice.model.HtmlSection;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */

@Component
public class HtmlSectionMapper {

	public HtmlSectionDTO toDto(final HtmlSection section) {
		HtmlSectionDTO sectionDto = new HtmlSectionDTO();
		BeanUtils.copyProperties(section, sectionDto);
		return sectionDto;
	}

	public HtmlSection toEntity(final HtmlSectionDTO sectionDto) {
		HtmlSection section = new HtmlSection();
		BeanUtils.copyProperties(sectionDto, section);
		return section;
	}
}