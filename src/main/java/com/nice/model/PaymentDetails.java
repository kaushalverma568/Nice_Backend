package com.nice.model;

import java.util.Date;

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
 * @date : 15-07-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "payment_details")
@Entity()
public class PaymentDetails extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 6090484153949186195L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "transaction_no", nullable = false, unique = true)
	private String transactionNo;

	@Column(name = "paid_on", nullable = false)
	private Date paidOn;

	@Column(name = "payment_amount", nullable = false)
	private Double paymentAmount;

	@JoinColumn(name = "delivery_boy_id", nullable = true)
	@ManyToOne(fetch = FetchType.LAZY)
	private DeliveryBoy deliveryBoy;

	@JoinColumn(name = "vendor_id", nullable = true)
	@ManyToOne(fetch = FetchType.LAZY)
	private Vendor vendor;

	@Column(name = "no_of_orders", nullable = false)
	private Integer noOfOrders;

}
