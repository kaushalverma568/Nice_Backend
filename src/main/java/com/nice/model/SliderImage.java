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
 * @date : 26-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "slider_image")
@Entity()
public class SliderImage extends CommonModel {
	/**
	 *
	 */
	private static final long serialVersionUID = -3291128134640861351L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "image_name")
	private String imageName;

	@Column(name = "origional_image_name")
	private String origionalImageName;

	@Column(name = "image_type", nullable = false)
	private String type;
}
