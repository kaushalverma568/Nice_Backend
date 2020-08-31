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
 * @date : 22-Jun-2020
 */
@Entity
@Table(name = "state")
@Data
@EqualsAndHashCode(callSuper = false)
public class State extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -6970796654901927274L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameArabic;

	@JoinColumn(name = "country_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Country country;

}