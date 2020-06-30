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

	@Column(name = "bank_name", nullable = false)
	private String bankName;

	@Column(name = "branch_name", nullable = false)
	private String branchName;

	@Column(name = "account_name", nullable = false)
	private String accountName;

	@Column(name = "account_number", nullable = false)
	private String accountNumber;

	@Column(name = "kib_no", nullable = false)
	private String kibNo;

	@Column(name = "branch_city", nullable = false)
	private String branchCity;
}
