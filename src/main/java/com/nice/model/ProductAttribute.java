package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Entity
@Table(name = "product_attribute")
@Data
@EqualsAndHashCode(callSuper = false)
public class ProductAttribute extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 2499566932816782817L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false)
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false)
	private String nameArabic;

	@Column(name = "description_english", nullable = false)
	private String descriptionEnglish;

	@Column(name = "description_arabic", nullable = false)
	private String descriptionArabic;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

}
