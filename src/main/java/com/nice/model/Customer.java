package com.nice.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
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
@Table(name = "customer")
@Data
@EqualsAndHashCode(callSuper = false)
public class Customer extends CommonModel {

	private static final long serialVersionUID = 3240506949329976947L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name", nullable = false)
	private String lastName;

	@Column(name = "email", nullable = false, unique = true)
	private String email;

	@Column(name = "phone_number")
	private String phoneNumber;

	@Column(name = "gender")
	private String gender;

	@Column(name = "registered_via", nullable = false)
	private String registeredVia;

	@Column(name = "email_verified", nullable = false)
	private Boolean emailVerified;

	@Column(name = "phone_verified")
	private Boolean phoneVerified;

	@Column(name = "status")
	private String status;

	@Temporal(TemporalType.DATE)
	@Column(name = "birth_date")
	private Date birthDate;

	@Column(name = "preferred_language")
	private String preferredLanguage;

	@Column(name = "wallet_amt")
	private Double walletAmt;
}