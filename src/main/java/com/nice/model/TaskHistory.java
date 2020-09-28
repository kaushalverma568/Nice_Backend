/**
 *
 */
package com.nice.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@Entity
@Table(name = "task_history")
@Data
@EqualsAndHashCode(callSuper = false)
public class TaskHistory extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -2365542616398144703L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST })
	@JoinColumn(name = "task_id", nullable = false)
	private Task task;

	@Column(name = "delivery_boy_id", nullable = true)
	private Long deliveryBoyId;

	@Column(name = "order_id", nullable = false)
	private Long orderId;

	@Column(name = "status", nullable = false)
	private String status;

	@Column(name = "order_delivery_type", nullable = false)
	private String orderDeliveryType;
	/**
	 * This contains if the order is replacement or delivery or return
	 */
	@Column(name = "task_type", nullable = false)
	private String taskType;
}
