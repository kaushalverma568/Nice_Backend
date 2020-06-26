/**
 *
 */
package com.nice.constant;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jun-2020
 */
@Getter
@AllArgsConstructor
public enum CustomerStatus {
	PENDING("Pending"), ACTIVE("Active"), DE_ACTIVE("De-active");

	String statusValue;

}
