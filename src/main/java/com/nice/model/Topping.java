/**
 *
 */
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
 * @date : 22-Jun-2020
 */

@Data
@Entity
@Table(name = "topping")
@EqualsAndHashCode(callSuper = false)
public class Topping extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -2926917971755924107L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name", nullable = false)
	private String name;

	@Column(name = "description", nullable = false)
	private String description;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "product_food_type")
	private String productFoodType;

}
