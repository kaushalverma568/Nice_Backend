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
@Table(name = "order_item_rating")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrderItemRating extends CommonModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2499566932816782817L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "order_Rating_id", nullable = false)
	private Long orderRatingId;
	
	@Column(name = "product_id", nullable = false)
	private Long productId;
	
	@Column(name = "item_rating", nullable = false)
	private Double itemRating;
		

}
