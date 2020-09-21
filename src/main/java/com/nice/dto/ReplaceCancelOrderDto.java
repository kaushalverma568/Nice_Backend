/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Apr-2020
 */
@Data
public class ReplaceCancelOrderDto implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8712932706410488373L;

	@NotNull(message = "{order.id.not.null}")
	private Long orderId;
	@NotNull(message = "{reason.required}")
	private Long reasonId;
	@NotBlank(message = "{description.required}")
	private String description;
}
