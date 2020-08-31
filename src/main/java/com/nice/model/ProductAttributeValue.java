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
@Table(name = "product_attribute_values")
@Data
@EqualsAndHashCode(callSuper = false)
public class ProductAttributeValue extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 2499566932816782817L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "attribute_value_english", nullable = false)
	private String attributeValueEnglish;

	@Column(name = "attribute_value_arabic", nullable = false)
	private String attributeValueArabic;

	@Column(name = "description_english", nullable = false)
	private String descriptionEnglish;

	@Column(name = "description_arabic", nullable = false)
	private String descriptionArabic;

	@Column(name = "rate")
	private Double rate;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	@JoinColumn(name = "product_variant_id", nullable = false)
	private ProductVariant productVariant;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.MERGE)
	@JoinColumn(name = "product_attribute_id", nullable = false)
	private ProductAttribute productAttribute;

	@Column(name = "discounted_rate", nullable = true)
	private Double discountedRate;

}
