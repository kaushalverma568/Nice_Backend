package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 19, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class TicketReasonDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6852468439640499935L;

	private Long id;

	@NotBlank(message = "{reason.not.null}")
	private String reason;

	@NotBlank(message = "{type.not.null}")
	private String type;

	@NotNull(message = "{active.not.null}")
	private Boolean active;
}