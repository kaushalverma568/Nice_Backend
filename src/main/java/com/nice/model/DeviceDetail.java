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
 * @date   : 28-Apr-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "device_detail")
@Entity()
public class DeviceDetail extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -5696167538439014738L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "device_id", nullable = false)
	private String deviceId;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", referencedColumnName = "id", nullable = false)
	private UserLogin userLogin;

	@Column(name = "user_type", nullable = false)
	private String userType;

	@Column(name = "device_type", nullable = false)
	private String deviceType;

}
