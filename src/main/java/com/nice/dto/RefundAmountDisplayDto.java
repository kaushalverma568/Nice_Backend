/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 13-Oct-2020
 */
@Data
public class RefundAmountDisplayDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -3300092325044065042L;
	private Double orderAmount;
	private Double maxRefundAmt;
}
