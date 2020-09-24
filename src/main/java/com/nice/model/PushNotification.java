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
 * @author : Kody Technolab PVT. LTD.
 * @date : 23-Sep-2020
 */
@Entity(name = "pushNotification")
@Data
@Table(name = "push_notification")
@EqualsAndHashCode(callSuper = false)
public class PushNotification extends CommonModel {
	/**
	 *
	 */
	private static final long serialVersionUID = -1544757984028083038L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "entity_id")
	private Long entityId;

	@Column(name = "entity_type")
	private String entityType;

	@Column(name = "order_id")
	private Long orderId;

	@Column(name = "message_english")
	private String messageEnglish;

	@Column(name = "message_arabic")
	private String messageArabic;
	/**
	 * future purpose only we have not implemented yet
	 */
	@Column(name = "success_all")
	private Boolean successAll;
}
