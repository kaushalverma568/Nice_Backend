package com.nice.model;

import java.math.BigDecimal;

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
 * @date : Jun 19, 2020
 */
@Entity
@Table(name = "delivery_boy_location")
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyLocation extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 4698538221311296723L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@JoinColumn(name = "delivery_boy_id", nullable = false)
	@OneToOne(fetch = FetchType.LAZY)
	private DeliveryBoy deliveryBoy;

	@Column(name = "latitude", nullable = false)
	private BigDecimal latitude;

	@Column(name = "longitude", nullable = false)
	private BigDecimal longitude;

}
