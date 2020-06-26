package com.nice.model;

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
 * @date : 26-06-2020
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

	private String name;

}
