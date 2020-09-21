/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
@Data
public class Notification implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 4067513787214340174L;

	private Long userId;

	private Long customerId;

	private String type;

	private String otp;

	private String userType;

	private String email;

	private Long vendorId;

	private String sendingType;
	
	private String language;
}
