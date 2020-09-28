package com.nice.model;

import java.math.BigDecimal;

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

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Entity
@Table(name = "online_cart")
@Data
@EqualsAndHashCode(callSuper = false)
public class OnlineCart extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -2450987913196485269L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "customer_id", referencedColumnName = "id", nullable = false)
	private Customer customer;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "product_variant_id", referencedColumnName = "id", nullable = false)
	private ProductVariant productVariant;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "first_name", nullable = true)
	private String firstName;

	@Column(name = "last_name", nullable = true)
	private String lastName;

	@Column(name = "address_english", nullable = true)
	private String addressEnglish;

	@Column(name = "address_arabic", nullable = true)
	private String addressArabic;

	@Column(name = "state_id", nullable = true)
	private Long stateId;

	@Column(name = "city_id", nullable = true)
	private Long cityId;

	@Column(name = "pincode_id", nullable = true)
	private Long pincodeId;

	@Column(name = "latitude", nullable = true)
	private BigDecimal latitude;

	@Column(name = "longitude", nullable = true)
	private BigDecimal longitude;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;
	/**
	 * status = Pending or Payment_Waiting
	 */
	@Column(name = "status", nullable = false)
	private String status;
	/**
	 * generated online order id at the time off placed order validation
	 */
	@Column(name = "online_order_id")
	private String onlineOrderId;

	@Column(name = "payment_token")
	private String paymentToken;

	@Column(name = "payment_id")
	private String paymentId;

	@Column(name = "administrative_charge")
	private Double administrativeCharge;

	@Column(name = "payment_amount")
	private Double paymentAmount;

	/**
	 * this will be either Pick-Up or Delivery
	 */
	@Column(name = "delivery_type", nullable = false)
	private String deliveryType;

	@Column(name = "description", nullable = true)
	private String description;

	/**
	 * Wallet contribution for order
	 */
	@Column(name = "wallet_contribution", nullable = true)
	private Double walletContirbution;
}
