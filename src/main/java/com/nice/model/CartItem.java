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
 * @date : 03-Jul-2020
 */
@Entity
@Table(name = "cart_item")
@Data
@EqualsAndHashCode(callSuper = false)
public class CartItem extends CommonModel {
	/**
	 *
	 */
	private static final long serialVersionUID = -9200586687208937424L;

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

	/**
	 * generated razor pay order id at the time off placed order validation
	 */
	@Column(name = "razor_pay_order_id")
	private String razorpayOrderId;
}
