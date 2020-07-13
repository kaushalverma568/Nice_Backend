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
 * @date : 23-Mar-2020
 */
@Entity
@Table(name = "orders_status_history")
@Data
@EqualsAndHashCode(callSuper = false)
public class OrderStatusHistory extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8102324091197462092L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "order_id", nullable = false)
	private Long orderId;

	@Column(name = "status", nullable = false)
	private String status;

}
