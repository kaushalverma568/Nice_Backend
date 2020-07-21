/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Data
public class CustomerAddressResponseDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = -3040732916895816507L;

	private Long id;
	private Long customerId;
	private String firstName;
	private String lastName;
	private String phoneNumber;
	private String streetNo;
	private String buildingName;
	private String landmark;
	private Long pincodeId;
	private String pincodeValue;
	private Long countryId;
	private String countryName;
	private Long stateId;
	private String stateName;
	private Long cityId;
	private String cityName;
	private Boolean active;
	private boolean defaultAddress;

}