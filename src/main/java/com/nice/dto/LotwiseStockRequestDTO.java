package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class LotwiseStockRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3557798897754609346L;
	@NotNull(message = "{total.qty.not.null}")
	private Long totalQty;
	@NotNull(message = "{lot.no.not.null}")
	private Long lotNo;
	@NotNull(message = "{expiryDate.not.null}")
	private Date expiryDate;
	@NotNull(message = "{lotDate.not.null}")
	private Date lotDate;
}
