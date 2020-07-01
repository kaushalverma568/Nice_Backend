package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.Data;

@Data
public class AddStockRequestDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5712519662964872811L;
	@NotNull(message = "{sku.not.null}")
	private String sku;

	@NotNull(message = "{vendor.id.not.null}")
	private Long vendorId;

	private List<LotwiseStockRequestDTO> stockRequestDTOs;
}
