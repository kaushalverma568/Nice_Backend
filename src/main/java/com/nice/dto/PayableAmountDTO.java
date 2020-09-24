package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Data
public class PayableAmountDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6085818929520663290L;

	@NotNull(message = "{task.ids.not.null}")
	private List<Long> taskIds;

	private String entityType;
}