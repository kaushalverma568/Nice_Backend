package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class OrderRatingDTO implements Serializable {/**
	 * 
	 */
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{orderId.not.null}")
	private Long orderId;
	
	@NotNull(message = "{packing.rating.not.null}")
	private Double orderPackingRating;
	
	@NotNull(message = "{money.rating.not.null}")
	private Double valueOfMoneyRating;
	
	@NotNull(message = "{boy.rating.not.null}")
	private Double deliveryBoyRating;
	
	private String review;
	
	@NotNull(message = "{active.not.null}")
	private Boolean active;
	
	private List<OrderItemRatingDTO> orderItemRatingList;

}
