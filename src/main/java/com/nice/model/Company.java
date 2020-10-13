package com.nice.model;

import java.math.BigDecimal;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Entity
@Table(name = "company")
@Data
@EqualsAndHashCode(callSuper = false)
public class Company extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1748457035872511471L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "name_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameEnglish;

	@Column(name = "name_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String nameArabic;

	@Column(name = "gstin", nullable = false)
	private String gstin;

	@Column(name = "company_email", nullable = false)
	private String companyEmail;

	@Column(name = "customer_care_email", nullable = false)
	private String customerCareEmail;

	@Column(name = "company_address_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String companyAddressEnglish;

	@Column(name = "company_address_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String companyAddressArabic;

	@Column(name = "phone_number", nullable = false)
	private String phoneNumber;

	@Column(name = "company_image_name")
	private String companyImageName;

	@Column(name = "company_image_original_name")
	private String companyImageOriginalName;

	@Column(name = "latitude")
	private BigDecimal latitude;

	@Column(name = "longitude")
	private BigDecimal longitude;

	@OneToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "area_id", referencedColumnName = "id", nullable = false)
	private Area area;

}