/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.ToString;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 14-07-2020
 */
@Data
@ToString
public class ToppingDTO implements Serializable {

	/**
	* 
	*/
	private static final long serialVersionUID = 3156141752740167407L;

	private Long id;

	@NotBlank(message = "{name.not.null}")
	private String name;

	@NotBlank(message = "{description.not.null}")
	private String description;

	private String productFoodType;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}
