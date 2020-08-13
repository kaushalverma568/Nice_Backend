/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Aug-2020
 */
@Data
public class DeliveryBoyActiveTimeDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7105647920338271344L;

	private Long id;
	private Boolean active;
	private Date recordDate;
	private Long deliveryBoyId;
	private Long activeTimeMinutes;
}
