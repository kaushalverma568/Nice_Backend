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
 * @date : 29-Jun-2020
 */
@Entity
@Table(name = "users")
@Data
@EqualsAndHashCode(callSuper = false)
public class Users extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -4038899957784498885L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "first_name_english", nullable = false)
	private String firstNameEnglish;

	@Column(name = "last_name_english", nullable = false)
	private String lastNameEnglish;

	@Column(name = "first_name_arabic", nullable = false)
	private String firstNameArabic;

	@Column(name = "last_name_arabic", nullable = false)
	private String lastNameArabic;

	@Column(name = "email", nullable = false, unique = true)
	private String email;

	@Column(name = "preferred_language")
	private String preferredLanguage;

	@JoinColumn(name = "role_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Role role;
}
