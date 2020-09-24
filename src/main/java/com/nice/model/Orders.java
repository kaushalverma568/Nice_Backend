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

@Entity
@Table(name = "orders")
@Data
@EqualsAndHashCode(callSuper = false)
public class Orders extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 63234495690384066L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "customer_id", nullable = false)
	private Customer customer;
	/**
	 * Contains Order Status
	 */
	@Column(name = "status", nullable = false)
	private String orderStatus;

	@Column(name = "payment_mode", nullable = false)
	private String paymentMode;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "address_english", nullable = false)
	private String addressEnglish;

	@Column(name = "address_arabic", nullable = false)
	private String addressArabic;

	@Column(name = "latitude", nullable = true)
	private BigDecimal latitude;

	@Column(name = "longitude", nullable = true)
	private BigDecimal longitude;

	@JoinColumn(name = "state_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	private State state;

	@JoinColumn(name = "city_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	private City city;

	@JoinColumn(name = "pincode_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	private Pincode pincode;

	@Column(name = "delivery_charge", nullable = true)
	private Double deliveryCharge;

	@Column(name = "total_order_amt", nullable = false)
	private Double totalOrderAmount;

	/**
	 * This field contains the amount of order after discount, not including
	 * delivery charge
	 */
	@Column(name = "gross_order_amount", nullable = false)
	private Double grossOrderAmount;

	@Column(name = "payment_id", nullable = true)
	private String paymentId;

	@Column(name = "online_order_id", nullable = true)
	private String onlineOrderId;

	@Column(name = "online_payment_token", nullable = true)
	private String onlinePaymentToken;

	@Column(name = "administrative_charge")
	private Double administrativeCharge;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "delivery_boy_id", nullable = true)
	private DeliveryBoy deliveryBoy;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "vendor_id", nullable = false)
	private Vendor vendor;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "return_replace_cancel_reject_reason_id", nullable = true)
	private TicketReason returnReplaceReason;

	@Column(name = "cancel_return_replace_description", nullable = true)
	private String cancelReturnReplaceDescription;

	@Column(name = "description", nullable = true)
	private String description;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "return_replace_delivery_boy_id", nullable = true)
	private DeliveryBoy replacementDeliveryBoy;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

	@Column(name = "discount_amount", nullable = true)
	private Double discountAmount;

	@Column(name = "replaced", nullable = false)
	private Boolean replaced;
	/**
	 * this will be either Pick-Up or Delivery
	 */
	@Column(name = "delivery_type", nullable = false)
	private String deliveryType;

	/**
	 * this property is used at the time of sending notification through scheduler
	 */
	@Column(name = "assignment_try_count", columnDefinition = "integer default 0")
	private Integer assignmentTryCount;

	/**
	 * this property is used for sending notification after 45 sec
	 */
	@Temporal(TemporalType.TIME)
	@Column(name = "notification_timer", columnDefinition = "TIME WITH TIME ZONE default CURRENT_TIME")
	private Date notificationTimer;

	@Column(name = "distance", nullable = false)
	private Double distance;

	@Column(name = "wallet_contribution", nullable = true)
	private Double walletContribution;

	@Column(name = "refunded", nullable = false, columnDefinition = "boolean default false")
	private Boolean refunded;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "delivery_date", nullable = true)
	private Date deliveryDate;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "payment_date", nullable = true)
	private Date paymentDate;
}
