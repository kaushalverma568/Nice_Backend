package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Entity
@Table(name = "delivery_boy")
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoy extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -7360212501347757602L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "first_name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String firstNameEnglish;

	@Column(name = "last_name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String lastNameEnglish;

	@Column(name = "first_name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String firstNameArabic;

	@Column(name = "last_name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String lastNameArabic;

	@Column(name = "email", nullable = false, unique = true)
	private String email;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

	@Column(name = "status", nullable = false)
	private String status;

	@Column(name = "gender")
	private String gender;

	@Column(name = "bank_name_english")
	private String bankNameEnglish;

	@Column(name = "bank_name_arabic")
	private String bankNameArabic;

	@Column(name = "branch_name_english")
	private String branchNameEnglish;

	@Column(name = "branch_name_arabic")
	private String branchNameArabic;

	@Column(name = "account_name_english")
	private String accountNameEnglish;

	@Column(name = "account_name_arabic")
	private String accountNameArabic;

	@Column(name = "bank_account_number")
	private String bankAccountNumber;

	@Column(name = "kib_no")
	private String kibNo;

	@Column(name = "branch_city_english")
	private String branchCityEnglish;

	@Column(name = "branch_city_arabic")
	private String branchCityArabic;

	@Column(name = "email_verified", nullable = false)
	private Boolean emailVerified;

	@Column(name = "phone_verified", nullable = false)
	private Boolean phoneVerified;

	@Column(name = "rating", nullable = false, columnDefinition = "numeric(10,2) DEFAULT 0")
	private Double rating;

	@Column(name = "no_of_rating", nullable = false, columnDefinition = "bigint DEFAULT 0")
	private Long noOfRating;

	@Column(name = "profile_picture_name")
	private String profilePictureName;

	@Column(name = "profile_picture_original_name")
	private String profilePictureOriginalName;

	@Column(name = "preferred_language")
	private String preferredLanguage;

}
