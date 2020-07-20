package com.nice.dto;

import java.io.Serializable;
import java.sql.Date;
import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class DiscountDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -938955587866004964L;

	private Long id;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	@NotNull(message = "{discount.rate.not.null}")
	private Double discountRate;

	@NotNull(message = "{start.date.not.null}")
	private Date startDate;

	@NotNull(message = "{end.date.not.null}")
	private Date endDate;

	@NotEmpty(message = "{product.id.list.not.null}")
	private List<Long> productIds;

	@NotNull(message = "{category.id.not.null}")
	private Long categoryId;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;
}