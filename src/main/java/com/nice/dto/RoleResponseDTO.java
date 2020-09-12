package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 07, 2020
 */
@Data
public class RoleResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6889199479899965066L;

	private Long id;

	private String name;

	private String createdBy;

	private Date createdAt;

	private String description;

	private String updatedBy;

	private Date updatedAt;

	private Boolean active;

	private Boolean isDefault;
}
