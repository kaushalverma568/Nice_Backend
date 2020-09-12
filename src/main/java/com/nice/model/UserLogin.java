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
 * @date   : 22-Jun-2020
 */
@Entity
@Table(name = "user_login")
@Data
@EqualsAndHashCode(callSuper = false)
public class UserLogin extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8712594030416874969L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "entity_id")
	private Long entityId;

	@Column(name = "entity_type")
	private String entityType;

	@Column(name = "email", nullable = false)
	private String email;

	@Column(name = "phone_number")
	private String phoneNumber;

	@Column(name = "password", nullable = false)
	private String password;

	@JoinColumn(name = "role_id", nullable = false, columnDefinition = "BIGINT default 1")
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Role role;

	@Column(name = "facebook_key")
	private String facebookKey;

	@Column(name = "google_key")
	private String googleKey;

	@Column(name = "otp")
	private String otp;

}
