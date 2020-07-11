/**
 *
 */
package com.nice.dto;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jun-2020
 */
public enum ProductFoodType {

	VEG(1), NON_VEG(3), EGG(2);

	Integer value;

	private ProductFoodType(final Integer value) {
		this.value = value;
	}

}
