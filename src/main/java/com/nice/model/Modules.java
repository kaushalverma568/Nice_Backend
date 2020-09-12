package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Table(name = "modules")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
public class Modules extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 9076892106979780306L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(name = "name", unique = true, nullable = false, columnDefinition = "CHARACTER VARYING(255) default ' '")
	private String name;

	/**
	 * It contains side bar portion's name of UI
	 */
	@Column(name = "parent_module_name", nullable = false, columnDefinition = "CHARACTER VARYING(255) default ' '")
	private String parentModuleName;

	/**
	 * It contains weather this module is available for new role or not
	 */
	@Column(name = "available_for_new_role", columnDefinition = "Boolean default false")
	private Boolean availableForNewRole;
}
