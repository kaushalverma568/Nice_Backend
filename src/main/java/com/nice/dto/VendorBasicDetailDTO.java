package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Aug 05, 2020
 */
@Data
public class VendorBasicDetailDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 4839760485007403687L;

	private Long id;

	private String firstName;

	private String lastName;

	private String storeName;

	private Boolean profileCompleted;

	private Boolean manageInventory;

	private Long businessCategoryId;

	private String businessCategoryName;

	private String status;

	private String email;

	private String preferredLanguage;
}
