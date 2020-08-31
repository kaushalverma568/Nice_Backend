/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */
@Data
public class HtmlSectionDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1486991350212315557L;

	private Long id;

	private String sectionValue;

	@NotBlank(message = "{section.text.english.not.null}")
	private String sectionValueEnglish;

	@NotBlank(message = "{section.text.arabic.not.null}")
	private String sectionValueArabic;

	@NotBlank(message = "{section.type.not.null}")
	private String sectionType;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}