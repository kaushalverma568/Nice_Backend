package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class TicketResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -8991171938989615951L;

	private Long id;

	private String ticketReason;

	private String userType;

	private String email;

	private String name;

	private String phoneNumber;

	private String ticketStatus;

	private List<String> nextStatus;

	private String description;

	private String comment;

	private Boolean active;

	private Date createdAt;

}
