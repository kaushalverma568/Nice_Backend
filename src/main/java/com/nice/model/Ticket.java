package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 09-07-2020
 */

@Entity
@Table(name = "ticket")
@Data
@EqualsAndHashCode(callSuper = false)
public class Ticket extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 4912128647155436378L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "email", nullable = false)
	private String email;

	@Column(name = "user_type", nullable = false)
	private String userType;

	@Column(name = "ticket_reason", nullable = false)
	private String ticketReason;

	@Column(name = "ticket_status", nullable = false)
	private String ticketStatus;

	private String description;

	private String comment;
}
