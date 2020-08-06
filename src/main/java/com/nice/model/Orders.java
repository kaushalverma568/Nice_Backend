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

	@Column(name = "address", nullable = false)
	private String address;

	@Column(name = "latitude", nullable = false)
	private BigDecimal latitude;

	@Column(name = "longitude", nullable = false)
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

	@Column(name = "gross_order_amount", nullable = false)
	private Double grossOrderAmount;

	@Column(name = "transaction_id", nullable = true)
	private String transactionId;

	@Column(name = "online_order_id", nullable = true)
	private String onlineOrderId;

	@Column(name = "online_signature", nullable = true)
	private String onlinePaymentSignature;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "delivery_boy_id", nullable = true)
	private DeliveryBoy deliveryBoy;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "vendor_id", nullable = false)
	private Vendor vendor;

	@Column(name = "cancel_reason", nullable = true)
	private String cancelReason;

	@Column(name = "return_replace_reason", nullable = true)
	private String returnReplaceReason;

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

}
