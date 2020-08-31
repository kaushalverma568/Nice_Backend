package com.nice.model;

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
import lombok.ToString;

@Entity
@Data
@EqualsAndHashCode(callSuper = false)
@ToString
@Table(name = "vendor_bank_details")
public class VendorBankDetails extends CommonModel {
	/**
	*
	*/
	private static final long serialVersionUID = -5524816390056962480L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@JoinColumn(name = "vendor_id", nullable = false)
	@OneToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	private Vendor vendor;

	@Column(name = "account_number", nullable = false)
	private String accountNumber;

	@Column(name = "bank_name_english")
	private String bankNameEnglish;

	@Column(name = "bank_name_arabic")
	private String bankNameArabic;

	@Column(name = "branch_name_english")
	private String branchNameEnglish;

	@Column(name = "branch_name_arabic")
	private String branchNameArabic;

	@Column(name = "account_name_english")
	private String accountNameEnglish;

	@Column(name = "account_name_arabic")
	private String accountNameArabic;

	@Column(name = "kib_no")
	private String kibNo;

	@Column(name = "branch_city_english")
	private String branchCityEnglish;

	@Column(name = "branch_city_arabic")
	private String branchCityArabic;

}
