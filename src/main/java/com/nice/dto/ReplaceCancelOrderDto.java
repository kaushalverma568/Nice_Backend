/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Apr-2020
 */
@Data
@EqualsAndHashCode
public class ReplaceCancelOrderDto implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8712932706410488373L;

	@NotNull(message = "{order.id.not.null}")
	private Long orderId;
	private List<Long> orderItem;
	@NotBlank(message = "{reason.required}")
	private String reason;
	@NotBlank(message = "{description.required}")
	private String description;
}