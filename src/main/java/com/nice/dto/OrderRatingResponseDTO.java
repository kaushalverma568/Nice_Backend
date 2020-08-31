package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class OrderRatingResponseDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8040711294987954136L;

	private Long id;

	private Long orderId;

	private Long vendorId;

	private String vendorName;

	private Long deliveryBoyId;

	private String deliveryBoyName;

	private String deliveryBoyNameEnglish;

	private String deliveryBoyNameArabic;

	private Double question1Rating;

	private Double question2Rating;

	private Double question3Rating;

	private Double question4Rating;

	private Double question5Rating;

	private Double deliveryBoyRating;

	private Double vendorRating;

	private Double avgOrderRating;

	private String review;
}
