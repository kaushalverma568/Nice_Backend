package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
public class PushNotification implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = 5585050256418671448L;

	private Long customerId;

	private List<Long> deliveryBoyIds;

	private Long orderId;

	private String type;

	private String deviceId;

}
