package com.nice.model;

import java.sql.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "discount")
@Entity()
public class Discount extends CommonModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5024042669762593749L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "status", nullable = false)
	private String status;

	@Column(name = "discount_rate", nullable = false)
	private Double discountRate;

	@Column(name = "start_date", nullable = false)
	private Date startDate;

	@Column(name = "end_date", nullable = false)
	private Date endDate;

	@Column(name = "category_id", nullable = false)
	private Long categoryId;
	
	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "total_products", nullable = true)
	private Integer totalProducts;
}
