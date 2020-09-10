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
 * @date : 28-Jan-2020
 */

@Entity
@Table(name = "stock_transfer")
@Data
@EqualsAndHashCode(callSuper = false)
public class StockTransfer extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8321363361424422271L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@JoinColumn(name = "stock_details_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private StockDetails stockDetails;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "transfered_to", nullable = true)
	private String transferedTo;

	@Column(name = "transfered_from", nullable = true)
	private String transferedFrom;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@Column(name = "is_manual")
	private Boolean isManual;

	@Column(name = "order_id")
	private Long orderId;

}
