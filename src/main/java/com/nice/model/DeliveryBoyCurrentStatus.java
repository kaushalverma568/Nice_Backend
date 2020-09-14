package com.nice.model;

import javax.persistence.CascadeType;
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
 * @date   : 15-07-2020
 */
@Entity
@Table(name = "delivery_boy_current_status")
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyCurrentStatus extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -2811014482932347841L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@OneToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "delivery_boy_id", referencedColumnName = "id", nullable = false, unique = true)
	private DeliveryBoy deliveryBoy;

	/**
	 * when delivery boy is logged in into system
	 */
	@Column(name = "is_login", nullable = false)
	private Boolean isLogin;

	/**
	 * when delivery boy accepts order it will be mark as isBusy
	 */
	@Column(name = "is_busy", nullable = false)
	private Boolean isBusy;

	/**
	 * when delivery boy is not available for taking any order it will be mark as isAvailable false
	 */
	@Column(name = "is_available", nullable = false)
	private Boolean isAvailable;
}
