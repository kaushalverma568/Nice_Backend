/**
 *
 */
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
@Table(name = "orders_product_attribute")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrdersProductAttributeValue extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -4864357055913548019L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "product_attribute_value_id", referencedColumnName = "id", nullable = false)
	private ProductAttributeValue productAttributeValue;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "order_item_id", referencedColumnName = "id", nullable = false)
	private OrdersItem orderItem;

	@Column(name = "attribute_name", nullable = false)
	private String attributeName;

	@Column(name = "attribute_value", nullable = false)
	private String attributeValue;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "amount", nullable = false)
	private Double amount;

	@Column(name = "discounted_amount", nullable = false)
	private Double discountedAmount;
}
