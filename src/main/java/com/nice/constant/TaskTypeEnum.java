/**
 *
 */
package com.nice.constant;

import lombok.AllArgsConstructor;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
@AllArgsConstructor
public enum TaskTypeEnum {
	DELIVERY("Delivery"), REPLACEMENT("Replacement"), RETURN("Return"), PICKUP("Pick-Up");

	private String taskValue;

	public String getTaskValue() {
		return taskValue;
	}
}
