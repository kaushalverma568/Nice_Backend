package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class CustomerResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6468554264495057699L;
	private Long id;
	private String firstName;
	private String lastName;
	private String email;
	private String phoneNumber;
	private String gender;
	private String registeredVia;
	private Boolean active;
	private Boolean emailVerified;
	private Boolean mobileVerified;
	private Date createdAt;
	private String status;
	private String type;
	private Date birthdate;
	private List<CustomerAddressResponseDTO> addressList;

}
