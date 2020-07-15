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

@Data
@Entity
@Table(name = "product_topping")
@EqualsAndHashCode(callSuper = false)
public class ProductTopping extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = 355492703180332337L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "rate", nullable = false)
	private Double rate;

	@JoinColumn(name = "product_variant_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	private ProductVariant productVariant;

	@JoinColumn(name = "topping_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	private Topping topping;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "discounted_rate", nullable = true)
	private Double discountedRate;
}
