package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   :29-Jun-2020
 */
@Data
public class OrderItemRatingResponseDTO implements Serializable {
	/**
	* 
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	private Long orderRatingId;

	private Long productId;

	private String productName;

	private Double itemRating;

}
