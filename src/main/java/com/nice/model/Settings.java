package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Table(name = "settings")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
@ToString
public class Settings extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 7632274806913992080L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", updatable = false, nullable = false)
	private Long id;

	@Column(name = "field_name", nullable = false, unique = true)
	private String fieldName;
	@Column(name = "field_value", nullable = false)
	private String fieldValue;
	@Column(name = "encrypted", nullable = false)
	private Boolean encrypted;

	/**
	 *
	 */
	public Settings() {
	}

	/**
	 * @param settingsId
	 * @param fieldName
	 * @param fieldValue
	 * @param encrypted
	 */
	public Settings(final Long id, final String fieldName, final String fieldValue, final Boolean encrypted) {
		super();
		this.id = id;
		this.fieldName = fieldName;
		this.fieldValue = fieldValue;
		this.encrypted = encrypted;
	}

}
