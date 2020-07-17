/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@Data
public class TaskFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6395018902613928027L;

	@NotNull(message = "{deliveryBoy.id.not.null}")
	private Long deliveryBoyId;

	@NotNull(message = "{task.status.required}")
	private String status;

	@NotNull(message = "{task.type.required}")
	private String taskType;

	private Date updatedAt;

	private String searchKeyWord;

}