/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Data
public class DeliveryBoyResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1897534413961189199L;

	private Long id;

	private String name;

	private String email;

	private String gender;

	private String phoneNumber;

	private String bankName;

	private String branchName;

	private String accountName;

	private String bankAccountNumber;

	private String kibNo;

	private String branchCity;

	private String profilePictureUrl;

	private Date registeredOn;

	private Boolean active;

	private Boolean emailVerified;

	private Boolean isBusy;

	private Double rating;

	private Long noOfRating;

}
