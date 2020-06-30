/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1897534413961189199L;

	private Long id;

	private String fullName;

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

	private Boolean isEmailVerified;

	private Boolean isBusy;

	private Double rating;

	private Long noOfRating;

}
