package com.nice.util;

import org.apache.commons.codec.binary.Base64;
import org.apache.http.impl.client.HttpClients;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.nice.constant.Constant;
import com.nice.dto.LoginResponse;
import com.nice.dto.SocialLoginDto;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class OauthTokenUtil {

	private OauthTokenUtil() {
		throw new IllegalStateException("Utility class");
	}

	public static LoginResponse getAuthToken(final String url, final SocialLoginDto socialLoginDto) throws ValidationException {

		RestTemplate restTemplate = null;
		LoginResponse result = null;
		MultiValueMap<String, String> map = null;
		HttpHeaders headers = null;

		try {

			ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

			restTemplate = new RestTemplate(requestFactory);

			String plainCreds = socialLoginDto.getClientId() + ":" + socialLoginDto.getClientSecret();
			byte[] plainCredsBytes = plainCreds.getBytes();
			byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
			String base64Creds = new String(base64CredsBytes);

			headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
			headers.add("Accept", MediaType.APPLICATION_JSON_VALUE);
			headers.add("Authorization", "Basic " + base64Creds);

			map = new LinkedMultiValueMap<>();
			map.add("grant_type", Constant.GRANT_TYPE);
			map.add("username", socialLoginDto.getEmail().concat("!!").concat("CUSTOMER").concat("#").concat(socialLoginDto.getRegisteredVia()));
			map.add("password", socialLoginDto.getUniqueId());

			HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);
			String outhURL = url + "oauth/token";
			ResponseEntity<LoginResponse> response = restTemplate.postForEntity(outhURL, request, LoginResponse.class);

			result = response.getBody();

		} catch (Exception e) {
			throw new ValidationException(e.getMessage());
		}
		return result;
	}

}
