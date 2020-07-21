package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
public class CustomerExport implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8506255005099448893L;

	private String name;

	private String email;

	private String phoneNumber;

	private String gender;

	private String registeredVia;

	private Date registeredOn;
}