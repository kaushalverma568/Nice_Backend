package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Data
public class CustomerResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6468554264495057699L;
	private Long id;
	private String firstName;
	private String lastName;
	private String name;
	private String email;
	private String phoneNumber;
	private String gender;
	private String registeredVia;
	private Boolean active;
	private Boolean emailVerified;
	private Boolean phoneVerified;
	private Date createdAt;
	private String status;
	private Date birthDate;
	private List<CustomerAddressResponseDTO> addressList;
	private Long userId;
	private String preferredLanguage;
}
