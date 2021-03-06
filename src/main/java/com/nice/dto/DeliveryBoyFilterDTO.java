package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 2, 2020
 */
@Data
public class DeliveryBoyFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7469868325296448510L;

	private String searchKeyword;

	private String status;

	private Boolean activeRecords;

	private String sortByDirection;

	private String sortByField;
}
