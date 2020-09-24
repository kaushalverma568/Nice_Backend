package com.nice.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.ColumnResult;
import javax.persistence.ConstructorResult;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;

import com.nice.dto.DeliveryBoyPayoutDTO;
import com.nice.dto.VendorPayoutDTO;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 15-07-2020
 */
@SqlResultSetMapping(name = "DeliveryBoyPayout", classes = { @ConstructorResult(targetClass = DeliveryBoyPayoutDTO.class, columns = {
		@ColumnResult(name = "delivery_boy_id", type = Long.class), @ColumnResult(name = "delivery_boy_name", type = String.class),
		@ColumnResult(name = "delivery_boy_phone_number", type = String.class), @ColumnResult(name = "registered_on", type = Date.class),
		@ColumnResult(name = "cart_orders", type = Integer.class), @ColumnResult(name = "replace_orders", type = Integer.class),
		@ColumnResult(name = "return_orders", type = Integer.class), @ColumnResult(name = "total_attened", type = Integer.class),
		@ColumnResult(name = "last_payment_on", type = Date.class), @ColumnResult(name = "total_paid", type = Double.class) }) })

@SqlResultSetMapping(name = "VendorPayout", classes = { @ConstructorResult(targetClass = VendorPayoutDTO.class, columns = {
		@ColumnResult(name = "vendor_id", type = Long.class), @ColumnResult(name = "vendor_name", type = String.class),
		@ColumnResult(name = "store_name", type = String.class), @ColumnResult(name = "business_category_name", type = String.class),
		@ColumnResult(name = "vendor_phone_number", type = String.class), @ColumnResult(name = "store_phone_number", type = String.class),
		@ColumnResult(name = "registered_on", type = Date.class), @ColumnResult(name = "business_category_id", type = Long.class),
		@ColumnResult(name = "cart_orders", type = Integer.class), @ColumnResult(name = "replace_orders", type = Integer.class),
		@ColumnResult(name = "return_orders", type = Integer.class), @ColumnResult(name = "total_attened", type = Integer.class),
		@ColumnResult(name = "last_payment_on", type = Date.class), @ColumnResult(name = "total_paid", type = Double.class) }) })

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
