package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class OrderRatingDTO implements Serializable {
	/**
	* 
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	@NotNull(message = "{orderId.not.null}")
	private Long orderId;

	@NotNull(message = "{question1.rating.not.null}")
	private Double question1Rating;

	@NotNull(message = "{question2.rating.not.null}")
	private Double question2Rating;

	@NotNull(message = "{question3.rating.not.null}")
	private Double question3Rating;

//	@NotNull(message = "{question4.rating.not.null}")
	private Double question4Rating;

//	@NotNull(message = "{question5.rating.not.null}")
	private Double question5Rating;

	private String review;

	private Boolean isRatingCalculated;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
