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
 * @date   : 24-Jul-2020
 */
@Entity
@Table(name = "order_rating")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrderRating extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 2499566932816782817L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "order_id", nullable = false)
	private Long orderId;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "delivery_boy_id")
	private Long deliveryBoyId;

	@Column(name = "question1_rating", nullable = false)
	private Double question1Rating;

	@Column(name = "question2_rating", nullable = false)
	private Double question2Rating;

	@Column(name = "question3_rating", nullable = false)
	private Double question3Rating;

	@Column(name = "question4_rating", nullable = false)
	private Double question4Rating;

	@Column(name = "question5_rating", nullable = false)
	private Double question5Rating;

	@Column(name = "delivery_boy_rating", nullable = false)
	private Double deliveryBoyRating;

	@Column(name = "vendor_rating", nullable = false)
	private Double vendorRating;

	@Column(name = "avg_order_rating", nullable = false)
	private Double avgOrderRating;

	@Column(name = "review", nullable = false)
	private String review;
	
	@Column(name = "is_rating_calculated")
	private Boolean isRatingCalculated;

}
