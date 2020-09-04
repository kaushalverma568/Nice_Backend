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
 * @author : Kody Technolab PVT. LTD.
 * @date : 04-Sep-2020
 */
@Entity
@Table(name = "vendor_payment")
@Data
@EqualsAndHashCode(callSuper = false)
public class VendorPayment extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "vendor_order_id", nullable = false)
	private String vendorOrderId;

	@JoinColumn(name = "vendor_id", nullable = false)
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private Vendor vendor;

	@JoinColumn(name = "subscription_plan_id", nullable = false)
	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private SubscriptionPlan subscriptionPlan;

	@Column(name = "payment_token")
	private String paymentToken;

	@Column(name = "payment_id")
	private String paymentId;

	@Column(name = "administrative_charge")
	private Double administrativeCharge;

	@Column(name = "status", nullable = false)
	private String status;

	@Column(name = "amount", nullable = false)
	private Double amount;

}
