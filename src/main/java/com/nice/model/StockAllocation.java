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
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Apr-2020
 */

@Entity
@Table(name = "stock_allocation")
@Data
@EqualsAndHashCode(callSuper = false)
public class StockAllocation extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = -1762172186879091303L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "stock_detail_id", nullable = false)
	private StockDetails stockDetails;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "delivery_boy_id", nullable = false)
	private DeliveryBoy deliveryBoy;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "order_item_id", nullable = false)
	private OrdersItem orderItem;

	@Column(name = "order_id", nullable = false)
	private Long orderId;

	@ManyToOne(fetch = FetchType.LAZY, cascade = CascadeType.PERSIST)
	@JoinColumn(name = "task_id", nullable = false)
	private Task task;
	/**
	 * This would contain if the stock is allocated for delivery or replacement.
	 */
	@Column(name = "allocated_for", nullable = false)
	private String allocatedFor;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

}
