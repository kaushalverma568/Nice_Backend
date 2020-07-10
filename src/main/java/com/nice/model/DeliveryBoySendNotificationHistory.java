package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 05-07-2020
 */
@Entity
@Table(name = "delivery_boy_send_notification_history")
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoySendNotificationHistory extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -5638426834333972561L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@JoinColumn(name = "delivery_boy_id", nullable = false, unique = true)
	@OneToOne(fetch = FetchType.LAZY)
	private DeliveryBoy deliveryBoy;

	@Column(name = "order_id")
	private Long orderId;

}
