package com.nice.model;

import java.util.List;

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
import javax.persistence.Transient;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Entity
@Table(name = "orders_item")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrdersItem extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8051473069908329249L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "order_id", nullable = false)
	private Orders order;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "product_variant_id", nullable = false)
	private ProductVariant productVariant;

	@Column(name = "unit_price", nullable = false)
	private Double unitPrice;

	@Column(name = "unit_price_after_discount", nullable = true)
	private Double unitPriceAfterDiscount;

	@Column(name = "total_discount_amt", nullable = true)
	private Double totalDiscountAmt;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "total_amt", nullable = false)
	private Double totalAmt;

	@Column(name = "amount_before_discount", nullable = false)
	private Double amountBeforeDiscount;

	@Transient
	private List<OrdersAddons> orderAddonsList;

	@Transient
	private List<OrdersExtras> orderExtrasList;

	@Transient
	private List<OrdersToppings> orderToppingsList;

	@Transient
	private List<OrdersProductAttributeValue> orderProductAttributeValuesList;

	@Column(name = "replace_requested", nullable = false)
	private Boolean replaceRequested;

	@Column(name = "return_requested", nullable = false)
	private Boolean returnRequested;

	@Column(name = "return_replace_quantity", nullable = true)
	private Long returnReplaceQuantity;
}
