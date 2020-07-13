/**
 *
 */
package com.nice.model;

import java.io.Serializable;

import lombok.Data;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 13-07-2020
 */
@Data
public class OnlineRequest implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 4832890500727021097L;

	private int amount;

	private String currencyCode;
}
