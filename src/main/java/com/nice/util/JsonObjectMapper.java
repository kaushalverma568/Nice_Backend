package com.nice.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;

/**
 *
 * @author jigar.shah
 *
 */
public class JsonObjectMapper {

	private static final ObjectMapper MAPPER = new ObjectMapper();

	private JsonObjectMapper() {
	}

	private static class Holder {

		private Holder() {
			throw new IllegalAccessError("Holder class");
		}

		private static final JsonObjectMapper INSTANCE = new JsonObjectMapper();
	}

	public static JsonObjectMapper getInstance() {
		return Holder.INSTANCE;
	}

	public ObjectMapper getObjectMapper() {
		return MAPPER;
	}

	public ObjectReader getObjectReader() {
		return MAPPER.reader();
	}

	public ObjectWriter getObjectWriter() {
		return MAPPER.writer();
	}
}
