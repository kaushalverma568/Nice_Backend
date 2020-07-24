/**
 *
 */
package com.nice.constant;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Getter
@AllArgsConstructor
public enum DeliveryBoyStatus {
	PENDING("Pending"), ACTIVE("Active"), DE_ACTIVE("De-active");

	String statusValue;

}
