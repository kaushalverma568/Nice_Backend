package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 28, 2020
 */
@Data
public class PushNotificationResponseDTO implements Serializable {
	/**
	 *
	 */
	private static final long serialVersionUID = -1544757984028083038L;

	private Long id;

	private String message;

	private String messageEnglish;

	private String messageArabic;

	private Date createdAt;

	private String module;
}
