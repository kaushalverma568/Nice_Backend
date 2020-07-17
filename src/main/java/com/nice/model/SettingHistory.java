package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Table(name = "setting_history")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
@ToString
public class SettingHistory extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 7632274806913992080L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", updatable = false, nullable = false)
	private Long id;

	@Column(name = "field_name", nullable = false)
	private String fieldName;
	
	@Column(name = "previous_field_value", nullable = false)
	private String previousFieldValue;
	
	@Column(name = "current_field_value", nullable = false)
	private String currentFieldValue;
	
	/**
	 * for response purpose
	 */
	@Transient
	private String userName;

}
