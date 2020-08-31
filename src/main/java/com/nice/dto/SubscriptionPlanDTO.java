package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : Apr 9, 2020
 */
@Data
public class SubscriptionPlanDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotBlank(message = "{name.english.not.null}")
	private String nameEnglish;

	@NotBlank(message = "{name.arabic.not.null}")
	private String nameArabic;

	private String descriptionEnglish;

	private String descriptionArabic;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{days.not.null}")
	private Integer days;

	@NotNull(message = "{amount.not.null}")
	private Double amount;

	private String name;

	private String description;

}
