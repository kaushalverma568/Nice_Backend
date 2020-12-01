/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@Data
public class TaskFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6395018902613928027L;

	private Long deliveryBoyId;

	private Long vendorId;

	private Long orderId;

	private List<String> statusList;

	private List<String> statusListNotIn;

	private String taskType;

	private Date updatedAt;

	private String searchKeyWord;

	private Date deliveredDate;

	private Date orderDate;

	private Date createdAt;

	private Boolean deliveryBoyPaymentPending;

	private Boolean vendorPaymentPending;

	private Date orderDateFrom;

	private Date orderDateTo;

	private List<String> orderStatusNotIn;

}