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
 * @date : 14-Jul-2020
 */
@Entity
@Table(name = "addons")
@Data
@EqualsAndHashCode(callSuper = false)
public class Addons extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -326881677689030031L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false)
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false)
	private String nameArabic;

	@Column(name = "description_english", nullable = false)
	private String descriptionEnglish;

	@Column(name = "description_arabic", nullable = false)
	private String descriptionArabic;

	@JoinColumn(name = "vendor_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY, cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	private Vendor vendor;
}
