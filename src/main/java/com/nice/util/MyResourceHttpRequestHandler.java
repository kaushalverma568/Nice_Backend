/**
 *
 */
package com.nice.util;

import java.io.File;
import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.resource.ResourceHttpRequestHandler;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Component
public class MyResourceHttpRequestHandler extends ResourceHttpRequestHandler {

	public static final String ATTR_FILE = MyResourceHttpRequestHandler.class.getName() + ".file";

	@Override
	protected Resource getResource(final HttpServletRequest request) throws IOException {

		final File file = (File) request.getAttribute(ATTR_FILE);
		return new FileSystemResource(file);
	}
}
