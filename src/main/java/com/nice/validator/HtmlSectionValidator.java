/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.HtmlSectionDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.HtmlSectionService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */

@Component
public class HtmlSectionValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */

	@Autowired
	private HtmlSectionService sectionService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return HtmlSectionDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {

		final HtmlSectionDTO sectionDTO = (HtmlSectionDTO) target;

		/**
		 * Check section duplication based on size
		 */

		if ((sectionDTO != null) && sectionService.isExists(sectionDTO)) {
			errors.rejectValue("sectionValue", "409", messageByLocaleService.getMessage("section.exists", null));
		}
	}
}