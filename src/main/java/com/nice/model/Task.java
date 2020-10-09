/**
 *
 */
package com.nice.model;

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

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@Entity
@Table(name = "Task")
@Data
@EqualsAndHashCode(callSuper = false)
public class Task extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1788804120186373658L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	/**
	 * Here nullable is made true for delivery boy, as we will be making task for the pickup order as well and in that case
	 * no delivery boy will be assigned, this is taken so as to facilitate the payout structure for vendor, so now both
	 * payout will happen from task table itself(delivery boy and vendor payout)
	 */
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "delivery_boy_id", nullable = true)
	private DeliveryBoy deliveryBoy;

	/**
	 * For any order vendor would always be present, delivery boy may not be present in task if it is a pickup order.
	 */
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "vendor_id", nullable = false)
	private Vendor vendor;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "order_id", nullable = false)
	private Orders order;

	@Column(name = "status", nullable = false)
	private String status;

	/**
	 * This contains if the order is replacement or delivery
	 */
	@Column(name = "task_type", nullable = false)
	private String taskType;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.MERGE, CascadeType.PERSIST })
	@JoinColumn(name = "delivery_boy_payment_details_id", nullable = true)
	private PaymentDetails deliveryBoyPaymentDetails;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.MERGE, CascadeType.PERSIST })
	@JoinColumn(name = "vendor_payment_details_id", nullable = true)
	private PaymentDetails vendorPaymentDetails;

	@Column(name = "total_order_amount", nullable = false)
	private Double totalOrderAmount;

	@Column(name = "delivery_charge", nullable = false)
	private Double deliveryCharge;

	@Column(name = "admin_commission", nullable = false, columnDefinition = "double default 0.0")
	private Double adminCommission;

	@Column(name = "vendor_payable_amt", nullable = false, columnDefinition = "double default 0.0")
	private Double vendorPayableAmt;

	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "delivered_date")
	private Date deliveredDate;

	@Column(name = "order_delivery_type", nullable = false)
	private String orderDeliveryType;

	@Column(name = "admin_profit", nullable = true)
	private Double adminProfit;

	@Column(name = "delivery_boy_profit", nullable = true)
	private Double deliveryBoyProfit;

	@Column(name = "vendor_profit", nullable = true)
	private Double vendorProfit;

	@Column(name = "admin_commission_rate")
	private Double adminCommissionRate;

	/**
	 * This field is used to display delivery charge in vendor payout. Because whichever we pay to delivery boy not that
	 * vendor will bare in case of return/replace and customer pay for order.
	 *
	 * Example : Order :100 +Customer Delivery charge 30 = 130 Paid by customer. but we are giving 20 to delivery boy. Hence
	 * Delivery charge will be 20 (which will get by Delivery boy) , Customer Delivery charge(For customer and Vendor)
	 *
	 */
	@Column(name = "customer_delivery_charge")
	private Double customerDeliveryCharge;
}
