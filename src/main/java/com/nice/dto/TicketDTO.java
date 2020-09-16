package com.nice.dto;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class TicketDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -9015689741910023457L;

	private Long id;

	@NotNull(message = "{ticket.reason.id.not.null}")
	private Long ticketReasonId;

	private String ticketStatus;

	private String description;
}
