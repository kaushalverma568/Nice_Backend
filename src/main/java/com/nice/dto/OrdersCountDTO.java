/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Map;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 17-07-2020
 */
@Data
public class OrdersCountDTO implements Serializable {

	private static final long serialVersionUID = 8228304549464569873L;

	private Long deliveryBoyId;
	/**
	 * regular orders ,return orders,replace orders separate counts
	 *
	 */
	private Map<String, Integer> ordersCountMap;

}
