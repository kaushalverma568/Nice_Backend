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
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
@Table(name = "scheduler_details")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
public class SchedulerDetails extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1911997813196642757L;
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;
	@Column(name = "name", nullable = false)
	private String name;

}
