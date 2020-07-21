package com.nice.dto;

import java.io.Serializable;
import java.sql.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class DiscountResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5269976868706071854L;

	private Long id;

	private String status;

	private Double discountRate;

	private java.util.Date discountDate;

	private Date startDate;

	private Date endDate;

	private Integer totalProducts;

	private String categoryName;

	private Boolean active;

	private Date createdAt;

}
