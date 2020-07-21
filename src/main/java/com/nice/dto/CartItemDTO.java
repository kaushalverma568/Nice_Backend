package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 03-Jul-2020
 */
@Data
public class CartItemDTO implements Serializable {

	/**
	*
	*/
	private static final long serialVersionUID = 2920180246659376736L;

	private Long id;

	private Long customerId;

	@NotNull(message = "{variant.id.not.null}")
	private Long productVariantId;

	@NotNull(message = "{quantity.not.null}")
	private Long quantity;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private List<Long> productAddonsId;

	private List<Long> productExtrasId;

	private List<Long> attributeValueIds;

	private List<Long> productToppingsIds;

	private String razorPayOrderId;
}