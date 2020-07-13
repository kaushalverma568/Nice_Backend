package com.nice.model;

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

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "address", nullable = false)
	private String address;

	@Column(name = "state_id", nullable = false)
	private Long stateId;

	@Column(name = "city_id", nullable = false)
	private Long cityId;

	@Column(name = "pincode_id", nullable = false)
	private Long pincodeId;

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

	@Column(name = "payment_amount")
	private Double paymentAmount;
}
