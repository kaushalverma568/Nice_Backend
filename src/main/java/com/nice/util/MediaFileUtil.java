package com.nice.util;

import org.springframework.http.MediaType;

/**
 * Determine file's content type
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class MediaFileUtil {
	private static final String PNG_EXT = ".png";
	private static final String JPG_EXT = ".jpg";
	private static final String JPEG_EXT = ".jpeg";
	private static final String GIF_EXT = ".gif";
	private static final String JPE_EXT = ".jpe";
	private static final String JFIF_EXT = ".jfif";
	private static final String PDF_EXT = ".pdf";

	private MediaFileUtil() {

	}

	public static MediaType getMediaType(String fileName) {
		fileName = fileName.toLowerCase();
		String extension = fileName.substring(fileName.lastIndexOf('.'));
		switch (extension) {
		case PNG_EXT:
			return MediaType.IMAGE_PNG;
		case JPG_EXT:
		case JPEG_EXT:
		case JPE_EXT:
		case JFIF_EXT:
			return MediaType.IMAGE_JPEG;
		case GIF_EXT:
			return MediaType.IMAGE_GIF;
		case PDF_EXT:
			return MediaType.APPLICATION_PDF;
		default:
			return MediaType.APPLICATION_OCTET_STREAM;
		}
	}
}