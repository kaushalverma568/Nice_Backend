package com.nice.constant;

import java.util.HashMap;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Getter
@AllArgsConstructor
public enum HtmlSections {

	RETURN("RETURN"), PRIVACY("PRIVACY"), TERM_CONDITION("TERM_CONDITION"), FAQ("FAQ");

	String statusValue;

	private static final Map<String, HtmlSections> SECTION_LIST = new HashMap<>();
	static {
		for (final HtmlSections section : values()) {
			SECTION_LIST.put(section.getStatusValue(), section);
		}
	}

	public static HtmlSections getByValue(final String value) {
		return SECTION_LIST.get(value);
	}

}
