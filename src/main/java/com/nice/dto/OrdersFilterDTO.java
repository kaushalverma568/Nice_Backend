/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
public class OrdersFilterDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 1481036194893196099L;

	private Long deliveryBoyId;

	private Long storeId;

	private String orderStatus;

	private Date orderDate;

	private String searchKeyword;
}
