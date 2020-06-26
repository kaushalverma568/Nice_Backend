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
 * @date   : 26-Jun-2020
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

	@Column(name = "name", nullable = false)
	private String name;

	@Column(name = "gstin", nullable = false)
	private String gstin;

	@Column(name = "company_email", nullable = false)
	private String companyEmail;

	@Column(name = "customer_care_email", nullable = false)
	private String customerCareEmail;

	@Column(name = "company_address", nullable = false)
	private String companyAddress;

	@Column(name = "contact_no", nullable = false)
	private String contactNo;

	@Column(name = "company_image_name")
	private String companyImageName;

	@Column(name = "company_image_original_name")
	private String companyImageOriginalName;

}