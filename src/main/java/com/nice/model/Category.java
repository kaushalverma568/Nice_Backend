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
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "category")
@Entity
public class Category extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -1700215024930869052L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name", nullable = false)
	private String name;

	@JoinColumn(name = "vendor_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Vendor vendor;

	@Column(name = "image")
	private String image;

	@Column(name = "image_original_name")
	private String imageOriginalName;

}
