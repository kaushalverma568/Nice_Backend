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
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Entity
@Table(name = "business_category")
@Data
@EqualsAndHashCode(callSuper = false)
public class BusinessCategory extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 2499566932816782817L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameArabic;

	@Column(name = "image_name", nullable = false)
	private String imageName;

	@Column(name = "original_image_name", nullable = false)
	private String originalImageName;

	@Column(name = "manage_inventory", nullable = false)
	private Boolean manageInventory;
}
