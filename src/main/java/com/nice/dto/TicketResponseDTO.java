package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 31-Jan-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
public class TicketResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8991171938989615951L;

	private Long id;

	private String ticketReason;

	private String userType;

	private String email;

	private String ticketStatus;

	private List<String> nextStatus;

	private String description;

	private String comment;

	private Boolean active;

	private Date createdAt;

}
