package com.nice.dto;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Data
public class GmailCredentials {
	private String userEmail;
	private String clientId;
	private String clientSecret;
	private String accessToken;
	private String refreshToken;

}