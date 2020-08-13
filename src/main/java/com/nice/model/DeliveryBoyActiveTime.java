package com.nice.model;

import java.util.Date;

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
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Entity
@Table(name = "DELIVERY_BOY_ACTIVE_TIME")
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyActiveTime extends CommonModel {

	private static final long serialVersionUID = 3240506949329976947L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(cascade = { CascadeType.MERGE, CascadeType.PERSIST }, fetch = FetchType.LAZY)
	@JoinColumn(name = "delivery_boy_id")
	private DeliveryBoy deliveryBoy;

	@Temporal(TemporalType.DATE)
	@Column(name = "record_date")
	private Date recordDate;

	@Column(name = "active_time_minutes")
	private Long activeTimeMinutes;

}