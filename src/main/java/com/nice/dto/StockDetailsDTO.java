package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 28-Jan-2020
 */
@Data
public class StockDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8845840813113451281L;
	private Long id;
	private Long lotNo;
	private Long productId;
	private String productName;
	private Long uomId;
	private String measurement;
	private Double available;
	private Double reserved;
	private Double delivered;
	private Double replaced;
	private Double returned;
	private String sku;
	private Date expiryDate;
	private Date lotDate;
	private Long productVariantId;
	private Double expired;
}
