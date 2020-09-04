/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 04-Apr-2020
 */
@Data
public class OrdersListDTOForDeliveryBoy implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 3671337851055556338L;

	private Long id;

	private String storeName;

	private String storeNameEnglish;

	private String storeNameArabic;

	private String taskStatus;

	private Long taskId;

	private Date orderDate;

	private Date deliveredDate;

	private String storeImageUrl;

	private Double orderAmount;
}
