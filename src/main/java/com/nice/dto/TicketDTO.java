package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotBlank;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 09-07-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class TicketDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -9015689741910023457L;

	private Long id;

	@NotBlank(message = "{ticket.reason.not.null}")
	private String ticketReason;

	private String ticketStatus;

	private String description;
}
