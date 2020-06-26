package com.nice.config;

import java.io.IOException;
import java.util.Map;

import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
public class CustomOauthExceptionSerializer extends StdSerializer<CustomOauthException> {

	/**
	 *
	 */
	private static final long serialVersionUID = -2138094565342152208L;

	public CustomOauthExceptionSerializer() {
		super(CustomOauthException.class);
	}

	@Override
	public void serialize(final CustomOauthException value, final JsonGenerator jsonGenerator, final SerializerProvider serializerProvider) throws IOException {
		jsonGenerator.writeStartObject();
		jsonGenerator.writeNumberField("status", HttpStatus.UNAUTHORIZED.value());
		jsonGenerator.writeStringField("message", value.getMessage());
		if (value.getAdditionalInformation() != null) {
			for (Map.Entry<String, String> entry : value.getAdditionalInformation().entrySet()) {
				String key = entry.getKey();
				String add = entry.getValue();
				jsonGenerator.writeStringField(key, add);
			}
		}
		jsonGenerator.writeEndObject();
	}
}
