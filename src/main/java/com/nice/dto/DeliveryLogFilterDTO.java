package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Sep-2020
 */
@Data
public class DeliveryLogFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -1701259786743845846L;
	private String searchKeyword;
	private Date fromDate;
	private Date toDate;
	private Long customerId;
	private Long vendorId;
	private String taskType;
}
