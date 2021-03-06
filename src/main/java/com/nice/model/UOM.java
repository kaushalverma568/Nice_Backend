package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
@Table(name = "uom")
@Entity()
public class UOM extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 6559973860428744128L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "measurement_english", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String measurementEnglish;

	@Column(name = "measurement_arabic", nullable = false, columnDefinition = "CHARACTER VARYING(255) DEFAULT ' '")
	private String measurementArabic;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

}
