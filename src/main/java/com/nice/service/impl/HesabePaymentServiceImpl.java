/**
 *
 */
package com.nice.service.impl;

import java.util.Map;

import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.google.gson.Gson;
import com.nice.dto.DecryptResponseDTO;
import com.nice.dto.EncryptResponseDTO;
import com.nice.service.HesabePaymentService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Aug-2020
 */
@Service(value = "hesabePaymentService")
@Transactional(rollbackFor = Throwable.class)
public class HesabePaymentServiceImpl implements HesabePaymentService {

	private static final Logger LOGGER = LoggerFactory.getLogger(HesabePaymentServiceImpl.class);

	@Value("${hesabe.merchantCode}")
	private String merchantCode;
	@Value("${hesabe.key}")
	private String key;
	@Value("${hesabe.iv}")
	private String iv;
	@Value("${hesabe.url}")
	private String hesabeUrl;
	@Value("${hesabe.accessCode}")
	private String accessCode;
	@Value("${hesabe.version}")
	private String version;
	@Value("${hesabe.paymentType}")
	private String paymentType;

	@Override
	public String createPaymentGateway(final String orderId, final Double amount, final String redirectUrl) {
		LOGGER.info("inside create hesabe payment url with orderId :{}", orderId);
		String encryptedResponse = encryptData(amount, orderId, redirectUrl);
		String checkOutResponse = checkOut(encryptedResponse);
		String decryptedResponse = decrypt(checkOutResponse);
		DecryptResponseDTO decryptResponseDTO = convertToDecryptedResponseDTO(decryptedResponse);
		Map<String, String> decryptJsonResponse = decryptResponseDTO.getResponse();
		EncryptResponseDTO encryptResponseDTO = convertToEncryptedResponseDTO(decryptJsonResponse.get("data"));
		String paymentRequest = encryptResponseDTO.getResponse().get("data");
		return hesabeUrl + "payment?data=" + paymentRequest;
	}

	private String encryptData(final Double amount, final String orderId, final String redirectUrl) {
		LOGGER.info("inside encrypt data for vendor");
		UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(hesabeUrl.concat("api/encrypt"));
		builder.queryParam("amount", amount);
		builder.queryParam("currency", "KWD");
		builder.queryParam("merchantCode", merchantCode);
		builder.queryParam("paymentType", paymentType);
		builder.queryParam("responseUrl", redirectUrl);
		builder.queryParam("failureUrl", redirectUrl);
		builder.queryParam("variable1", orderId);
		builder.queryParam("version", version);
		HttpHeaders headers = new HttpHeaders();
		headers.add("primary", key);
		headers.add("secondary", iv);
		ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

		RestTemplate restTemplate = new RestTemplate(requestFactory);

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(builder.build().getQueryParams(), headers);
		ResponseEntity<String> result = restTemplate.postForEntity(builder.build().toUri(), request, String.class);
		EncryptResponseDTO encryptResponseDTO = convertToEncryptedResponseDTO(result.getBody());
		Map<String, String> encryptedResponse = encryptResponseDTO.getResponse();
		LOGGER.info("outside encrypt data for vendor");
		return encryptedResponse.get("data");
	}

	private String checkOut(final String encryptedData) {
		LOGGER.info("inside checkout data");
		UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(hesabeUrl.concat("checkout"));
		builder.queryParam("data", encryptedData);
		HttpHeaders headers = new HttpHeaders();
		headers.add("accessCode", accessCode);
		ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

		RestTemplate restTemplate = new RestTemplate(requestFactory);

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(builder.build().getQueryParams(), headers);
		ResponseEntity<String> result = restTemplate.postForEntity(builder.build().toUri(), request, String.class);
		LOGGER.info("outside checkout data");
		return result.getBody();
	}

	@Override
	public String decrypt(final String encrypted) {
		LOGGER.info("inside decrypt data");
		UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(hesabeUrl.concat("api/decrypt"));
		builder.queryParam("data", encrypted);
		HttpHeaders headers = new HttpHeaders();
		headers.add("primary", key);
		headers.add("secondary", iv);
		ClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(HttpClients.createDefault());

		RestTemplate restTemplate = new RestTemplate(requestFactory);

		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(builder.build().getQueryParams(), headers);
		ResponseEntity<String> result = restTemplate.postForEntity(builder.build().toUri(), request, String.class);
		LOGGER.info("outside decrypt data");
		return result.getBody();
	}

	private EncryptResponseDTO convertToEncryptedResponseDTO(final String encryptResponse) {
		Gson gson = new Gson();
		return gson.fromJson(encryptResponse, EncryptResponseDTO.class);
	}

	private DecryptResponseDTO convertToDecryptedResponseDTO(final String decryptResponse) {
		Gson gson = new Gson();
		return gson.fromJson(decryptResponse, DecryptResponseDTO.class);
	}

}
