package com.nice.model;

import java.math.BigDecimal;
import java.util.Date;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
@ToString
@Table(name = "vendor")
public class Vendor extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = 6835208341483221514L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@JoinColumn(name = "business_category_id", nullable = false)
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private BusinessCategory businessCategory;

	@Column(name = "email", nullable = false, unique = true)
	private String email;

	@Column(name = "is_email_verified", nullable = false)
	private Boolean isEmailVerified;

	@Column(name = "is_order_service_enable", nullable = false)
	private Boolean isOrderServiceEnable;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "store_name", nullable = false)
	private String storeName;

	@Column(name = "contact_no", nullable = false)
	private String contactNo;

	@JoinColumn(name = "subscription_plan_id")
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private SubscriptionPlan subscriptionPlan;

	@Temporal(TemporalType.DATE)
	@Column(name = "subscription_plan_start_date")
	private Date subscriptionPlanStartDate;

	@Temporal(TemporalType.DATE)
	@Column(name = "subscription_plan_end_date")
	private Date subscriptionPlanEndDate;

	@Column(name = "status", nullable = false)
	private String status;

	// this will be in minutes
	@Column(name = "approx_delivery_time")
	private String approxDeliveryTime;

	@Column(name = "minimum_order_amt")
	private Double minimumOrderAmt;

	@Column(name = "delivery_fee")
	private Double deliveryFee;

	// this will be online,offline or both
	@Column(name = "payment_method")
	private String paymentMethod;

	@Column(name = "building", nullable = false)
	private String building;

	@Column(name = "block", nullable = false)
	private String block;

	@Column(name = "street", nullable = false)
	private String street;

	@Column(name = "area", nullable = false)
	private String area;

	// this will be either pickup,delivery or both
	@Column(name = "delivery_type")
	private String deliveryType;

	@Column(name = "opening_hours")
	private String openingHours;

	@JoinColumn(name = "country_id")
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private Country country;

	@JoinColumn(name = "city_id")
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private City city;

	@JoinColumn(name = "pincode_id")
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private Pincode pincode;

	@Column(name = "latitude")
	private BigDecimal latitude;

	@Column(name = "longitude")
	private BigDecimal longitude;

	@Column(name = "rating")
	private Double rating;

	@Column(name = "no_of_rating")
	private Long noOfRating;
	/**
	 * it will accepts either replacement or return
	 */
	@Column(name = "accepts")
	private String accepts;

	@Column(name = "profile_picture_name")
	private String profilePictureName;

	@Column(name = "profile_picture_original_name")
	private String profilePictureOriginalName;

}
