package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class OrderItemRatingDTO implements Serializable {
	/**
	* 
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	@NotNull(message = "{order.rating.id.not.null}")
	private Long orderRatingId;

	@NotNull(message = "{product.id.not.null}")
	private Long productId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{item.rating.not.null}")
	private Double itemRating;

}
