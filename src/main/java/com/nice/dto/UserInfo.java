/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class UserInfo implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8463294896423919303L;

	private Long id;

	private Long entityId;

	private String entityType;

	private Long storeId;

	private String email;

	private String role;

	private String firstName;

	private String lastName;

	private String storeName;

	private Boolean canChangePassword;

}
