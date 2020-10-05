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

	private String nameEnglish;

	private String nameArabic;

	private String firstName;

	private String firstNameEnglish;

	private String firstNameArabic;

	private String lastName;

	private String lastNameEnglish;

	private String lastNameArabic;

	private String email;

	private String gender;

	private String phoneNumber;

	private String preferredLanguage;

	private String bankName;

	private String bankNameEnglish;

	private String bankNameArabic;

	private String accountName;

	private String accountNameEnglish;

	private String accountNameArabic;

	private String branchName;

	private String branchNameEnglish;

	private String branchNameArabic;

	private String bankAccountNumber;

	private String kibNo;

	private String branchCity;

	private String branchCityEnglish;

	private String branchCityArabic;

	private String profilePictureUrl;

	private Date registeredOn;

	private Boolean active;

	private Boolean emailVerified;

	private Boolean phoneVerified;

	private Boolean isBusy;

	private Double rating;

	private Long noOfRating;

	private String status;

	/**
	 * added for sending verification email
	 */
	private Long userId;
}
