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
@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "ticket_reason")
@Entity()
public class TicketReason extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8122529848270680042L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "reason", nullable = false)
	private String reason;

	@Column(name = "type", nullable = false)
	private String type;

}
