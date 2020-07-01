package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;
import lombok.ToString;

@Data
@ToString
public class StockDetailFilterDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -399312241457892556L;
	private Long productId;
	private String productName;
	private Long uomId;
	private Long vendorId;
	private Long lotNo;
	private Date expiryDate;
	private Date cratedDate;
	private String seachLotAndProductName;
	private String sku;
	private Boolean active;
}
