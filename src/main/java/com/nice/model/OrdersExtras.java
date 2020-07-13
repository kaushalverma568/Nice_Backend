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
 * @date : 02-Jul-2020
 */
@Entity
@Table(name = "orders_extras")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrdersExtras extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -3448431091247119590L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "product_extras_id", referencedColumnName = "id", nullable = false)
	private ProductExtras productExtras;

	@Column(name = "extras_name", nullable = false)
	private String extrasName;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "order_item_id", referencedColumnName = "id", nullable = false)
	private OrdersItem orderItem;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "amount", nullable = false)
	private Double amount;

	@Column(name = "discounted_amount", nullable = false)
	private Double discountedAmount;
}
