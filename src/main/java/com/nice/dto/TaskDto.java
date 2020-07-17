/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 10-Apr-2020
 */
@Data
@EqualsAndHashCode
public class TaskDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -9003260104567599744L;
	private Long id;
	private Long deliveryBoyId;
	@NotNull(message = "{order.not.null }")
	private Long orderId;
	private String status;
	@NotNull(message = "{task.type.not.null}")
	private String taskType;
}
