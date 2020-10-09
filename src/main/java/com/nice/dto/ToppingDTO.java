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
 * @date : 20-Jul-2020
 */
@Data
public class ToppingDTO implements Serializable {

	/**
	*
	*/
	private static final long serialVersionUID = 3156141752740167407L;

	private Long id;

	private String name;

	private String description;

	@NotBlank(message = "{english.name.not.null}")
	private String nameEnglish;

	private String descriptionEnglish;

	@NotBlank(message = "{arabic.name.not.null}")
	private String nameArabic;

	private String descriptionArabic;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	private String productFoodType;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
