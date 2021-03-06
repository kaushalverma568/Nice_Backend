package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Apr 9, 2020
 */
@Data
public class RatingQuestionDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotBlank(message = "{question.not.null}")
	private String questionEnglish;

	@NotBlank(message = "{question.not.null}")
	private String questionArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{type.not.null}")
	private String type;

	/**
	 * For response purpose only
	 */
	private String question;

}
