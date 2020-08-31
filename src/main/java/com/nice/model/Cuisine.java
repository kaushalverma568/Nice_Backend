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
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Entity
@Table(name = "cuisine")
@Data
@EqualsAndHashCode(callSuper = false)
public class Cuisine extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 8212277661872636386L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameArabic;

	@Column(name = "image_name")
	private String imageName;

	@Column(name = "image_original_name")
	private String imageOriginalName;

}
