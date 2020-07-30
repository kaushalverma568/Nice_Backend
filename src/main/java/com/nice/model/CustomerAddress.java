/**
 *
 */
package com.nice.model;

import java.math.BigDecimal;

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
 * @date : 22-Jun-2020
 */
@Entity
@Table(name = "customer_address")
@Data
@EqualsAndHashCode(callSuper = false)
public class CustomerAddress extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 4006462135017270871L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "customer_id", referencedColumnName = "id")
	private Customer customer;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

	@Column(name = "street_no", nullable = false)
	private String streetNo;

	@Column(name = "building_name", nullable = false)
	private String buildingName;

	@Column(name = "landmark", nullable = false)
	private String landmark;

	@JoinColumn(name = "pincode_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Pincode pincode;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	@JoinColumn(name = "country_id", nullable = false)
	private Country country;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE }, optional = true)
	@JoinColumn(name = "state_id")
	private State state;

	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE }, optional = true)
	@JoinColumn(name = "city_id")
	private City city;

	@Column(name = "default_address")
	private boolean defaultAddress;

	@Column(name = "latitude")
	private BigDecimal latitude;

	@Column(name = "longitude")
	private BigDecimal longitude;

	/**
	 * it could be either Home or Work
	 */
	@Column(name = "address_of", nullable = false)
	private String addressOf;
}