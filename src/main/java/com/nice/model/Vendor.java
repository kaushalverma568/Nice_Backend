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
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 29-06-2020
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

	@Column(name = "email_verified", nullable = false)
	private Boolean emailVerified;

	@Column(name = "is_order_service_enable", nullable = false)
	private Boolean isOrderServiceEnable;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "store_name", nullable = false)
	private String storeName;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

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

	/**
	 * maximum time for which vendor can accept return/replace (in days)
	 */
	@Column(name = "max_days_for_accept")
	private Integer maxDaysForAccept;

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

	@Temporal(TemporalType.TIME)
	@Column(name = "opening_hours_from")
	private Date openingHoursFrom;

	@Temporal(TemporalType.TIME)
	@Column(name = "opening_hours_to")
	private Date openingHoursTo;

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

	@Column(name = "store_image_name")
	private String storeImageName;

	@Column(name = "store_image_original_name")
	private String storeImageOriginalName;

	@Column(name = "featured_image_name")
	private String featuredImageName;

	@Column(name = "featured_image_original_name")
	private String featuredImageOriginalName;

	@Column(name = "store_detail_image_name")
	private String storeDetailImageName;

	@Column(name = "store_detail_image_original_name")
	private String storeDetailImageOriginalName;

	@Column(name = "phone_verified", nullable = false)
	private Boolean phoneVerified;

	@Column(name = "store_phone_number")
	private String storePhoneNumber;

	@Column(name = "is_featured", nullable = false)
	private Boolean isFeatured;
}
