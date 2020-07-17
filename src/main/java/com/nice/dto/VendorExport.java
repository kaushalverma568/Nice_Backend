package com.nice.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class VendorExport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8506255005099448893L;

	private String firstName;
	
	private String lastName;

	private String email;

	private String storeName;
	
	private String contactNo;
	
}