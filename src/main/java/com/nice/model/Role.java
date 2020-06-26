package com.nice.model;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Table(name = "role")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
public class Role extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -7915137780231594263L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	private String name;

	private String description;

}
