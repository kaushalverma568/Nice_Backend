package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class TempCartItemDTO implements Serializable {

	/**
	*
	*/
	private static final long serialVersionUID = 2920180246659376736L;

	private Long id;

	@NotBlank(message = "{uuid.not.null}")
	private String uuid;

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
}