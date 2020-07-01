package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;
import lombok.EqualsAndHashCode;
/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class OrderRatingResponseDTO implements Serializable {/**
	 * 
	 */
	private static final long serialVersionUID = 8040711294987954136L;

	
	private Long id;

	private Long orderId;

	private Long vendorId;
	
	private String vendorName;
	
	private Long deliveryBoyId;
	
	private String deliveryBoyName;
	
	private Double orderPackingRating;
	
	private Double valueOfMoneyRating;
	
	private Double deliveryBoyRating;
	
	private Double restaurantRating;
	
	private Double foodQualityRating;
	
	private String review;
	
	private List<OrderItemRatingResponseDTO> orderItemRatingDtoList;
}
