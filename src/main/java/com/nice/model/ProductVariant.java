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
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Entity(name = "productVariant")
@Data
@Table(name = "product_variant")
@EqualsAndHashCode(callSuper = false)
public class ProductVariant extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = -6313545987922656096L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "product_id", nullable = false)
	private Product product;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "uom_id", nullable = false)
	private UOM uom;

	@Column(name = "rate", nullable = false)
	private Double rate;

	@Column(name = "discounted_rate", nullable = true)
	private Double discountedRate;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "product_available", nullable = true)
	private Boolean productAvailable;

	@Column(name = "sku", nullable = true)
	private String sku;// if inventory to be mantained.

}
