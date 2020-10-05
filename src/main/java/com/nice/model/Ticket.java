package com.nice.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityResult;
import javax.persistence.FetchType;
import javax.persistence.FieldResult;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 09-07-2020
 */
@SqlResultSetMapping(name = "TicketMapping", entities = { @EntityResult(entityClass = Ticket.class, fields = { @FieldResult(name = "id", column = "id"),
		@FieldResult(name = "active", column = "active"), @FieldResult(name = "createdAt", column = "created_at"),
		@FieldResult(name = "updatedAt", column = "updated_at"), @FieldResult(name = "createdBy", column = "created_by"),
		@FieldResult(name = "updatedBy", column = "updated_by"), @FieldResult(name = "entityId", column = "entity_id"),
		@FieldResult(name = "userType", column = "user_type"), @FieldResult(name = "ticketReason", column = "ticket_reason_id"),
		@FieldResult(name = "ticketStatus", column = "ticket_status"), @FieldResult(name = "description", column = "description"),
		@FieldResult(name = "comment", column = "comment"), @FieldResult(name = "acknowledgeComment", column = "acknowledge_comment") }) })
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

	@Column(name = "entity_id", nullable = false)
	private Long entityId;

	@Column(name = "user_type", nullable = false)
	private String userType;

	@JoinColumn(name = "ticket_reason_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private TicketReason ticketReason;

	@Column(name = "ticket_status", nullable = false)
	private String ticketStatus;

	private String description;

	private String comment;

	private String acknowledgeComment;
}
