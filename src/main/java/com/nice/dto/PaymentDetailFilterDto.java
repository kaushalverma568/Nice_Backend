/**
 *
 */
package com.nice.dto;

import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@Data
public class PaymentDetailFilterDto {

	private Long deliveryBoyId;
	private Date fromDate;
	private Date toDate;
	/**
	 * This contains Delivery, replacement and return
	 */
	private String type;
}
