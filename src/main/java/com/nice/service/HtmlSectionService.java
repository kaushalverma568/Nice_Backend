/**
 *
 */
package com.nice.service;

import com.nice.dto.HtmlSectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.HtmlSection;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public interface HtmlSectionService {

	HtmlSection addText(HtmlSectionDTO sectionDto) throws ValidationException;

	HtmlSection updateText(HtmlSectionDTO sectionDto) throws NotFoundException, ValidationException;

	HtmlSectionDTO getText(String type) throws NotFoundException;

	boolean isExists(HtmlSectionDTO sectionDto);
}