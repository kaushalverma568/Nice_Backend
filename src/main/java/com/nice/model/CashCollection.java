/**
 *
 */
package com.nice.model;

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
 * @author : Kody Technolab PVT. LTD.
 * @date   : 15-Jul-2020
 */
@Entity
@Table(name = "cash_collection")
@Data
@EqualsAndHashCode(callSuper = false)
public class CashCollection extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1788804120186373658L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "delivery_boy_id", nullable = false)
	private DeliveryBoy deliveryBoy;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "order_id", nullable = false)
	private Orders order;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", nullable = false, unique = true)
	private Task task;

	@Column(name = "amount", nullable = false)
	private Double amount;
}
