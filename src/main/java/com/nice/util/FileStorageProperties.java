package com.nice.util;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@ConfigurationProperties(prefix = "file")
public class FileStorageProperties {
	private String uploadDir;

	public String getUploadDir() {
		return uploadDir;
	}

	public void setUploadDir(final String uploadDir) {
		this.uploadDir = uploadDir;
	}
}
