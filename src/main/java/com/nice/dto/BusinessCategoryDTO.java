package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class BusinessCategoryDTO implements Serializable {/**
	 * 
	 */
	private static final long serialVersionUID = 8040711294987954136L;
	
	private Long id;

	@NotNull(message = "{name.not.null}")
	private String name;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
	
	@NotNull(message = "{manage.inventory.not.null}")
	private Boolean manageInventory;
	
	private String imageUrl;

}
