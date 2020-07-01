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
 * @author nisha.parmar
 *
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
	
	@Column(name = "order_packing_rating", nullable = false)
	private Double orderPackingRating;
	
	@Column(name = "food_quality_rating", nullable = false)
	private Double foodQualityRating;
	
	@Column(name = "delivery_boy_rating", nullable = false)
	private Double deliveryBoyRating;
	
	@Column(name = "value_for_money_rating", nullable = false)
	private Double valueOfMoneyRating;
	
	@Column(name = "restaurant_rating", nullable = false)
	private Double restaurantRating;
	
	@Column(name = "review", nullable = false)
	private String review;
	
	

}
