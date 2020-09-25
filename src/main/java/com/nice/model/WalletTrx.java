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
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Entity
@Table(name = "wallet_trx")
@Data
@EqualsAndHashCode(callSuper = false)
public class WalletTrx extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8321363361424422271L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "amount", nullable = false)
	private Double amount;

	@JoinColumn(name = "order_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private Orders order;

	@JoinColumn(name = "customer_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private Customer customer;

	@Column(name = "description", nullable = true)
	private String description;

	@Column(name = "transaction_type", nullable = false)
	private String transactionType;
}
