/**
 *
 */
package com.nice.dto;

import com.nice.model.CommonModel;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class Notification extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 553712116319680610L;

	private Long userId;

	private Long customerId;

	private String type;

	private String otp;

	private String userType;

	private String email;

	private Long vendorId;
}
