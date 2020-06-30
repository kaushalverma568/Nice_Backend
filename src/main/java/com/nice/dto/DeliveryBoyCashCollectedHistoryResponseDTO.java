/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 24, 2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DeliveryBoyCashCollectedHistoryResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 4933469407589962975L;

	private Long orderId;

	private String customerName;

	private Double totalAmount;

	private Date collectedOn;

}