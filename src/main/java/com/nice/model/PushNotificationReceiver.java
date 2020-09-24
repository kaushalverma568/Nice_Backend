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
@Entity(name = "pushNotificationReceiver")
@Data
@Table(name = "push_notification_receiver")
@EqualsAndHashCode(callSuper = false)
public class PushNotificationReceiver extends CommonModel {
	/**
	 *
	 */
	private static final long serialVersionUID = -1544757984028083038L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "push_notification_id")
	private Long pushNotificationId;

	@Column(name = "sender_id")
	private Long senderId;

	@Column(name = "receiver_id")
	private Long receiverId;

	@Column(name = "device_id")
	private String deviceId;
	/**
	 * future purpose only we have not implemented yet
	 */
	@Column(name = "success")
	private Boolean success;

	@Column(name = "message_english")
	private String messageEnglish;

	@Column(name = "message_arabic")
	private String messageArabic;
}
