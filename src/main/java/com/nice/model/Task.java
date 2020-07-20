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
 *
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

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "delivery_boy_id", nullable = false)
	private DeliveryBoy deliveryBoy;

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
	@JoinColumn(name = "payment_details_id", nullable = true)
	private PaymentDetails paymentDetails;

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

}
