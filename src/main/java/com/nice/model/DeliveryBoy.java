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
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
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

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "email", nullable = false, unique = true)
	private String email;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

	@Column(name = "gender")
	private String gender;

	@Column(name = "bank_name")
	private String bankName;

	@Column(name = "branch_name")
	private String branchName;

	@Column(name = "account_name")
	private String accountName;

	@Column(name = "bank_account_number")
	private String bankAccountNumber;

	@Column(name = "kib_no")
	private String kibNo;

	@Column(name = "branch_city")
	private String branchCity;

	@Column(name = "is_email_verified", nullable = false)
	private Boolean isEmailVerified;

	@Column(name = "is_phone_number_verified", nullable = false)
	private Boolean isPhoneNumberVerified;

	@Column(name = "is_login", nullable = false)
	private Boolean isLogin;

	/**
	 * when delivery boy accepts order it will be mark as isBusy
	 */
	@Column(name = "is_busy", nullable = false)
	private Boolean isBusy;

	@Column(name = "rating", nullable = false)
	private Double rating;

	@Column(name = "no_of_rating", nullable = false)
	private Long noOfRating;

	@Column(name = "profile_picture_name")
	private String profilePictureName;

	@Column(name = "profile_picture_original_name")
	private String profilePictureOriginalName;
}
