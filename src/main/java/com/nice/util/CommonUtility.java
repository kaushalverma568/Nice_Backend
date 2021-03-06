package com.nice.util;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.security.SecureRandom;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 19-Jun-2020
 */
@Component
public class CommonUtility {

	@Autowired
	private CommonUtility() {
		super();
	}

	public static final Predicate<String> NOT_NULL_NOT_EMPTY_STRING = s -> s != null && !s.isEmpty();

	public static final Predicate<String> NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING = s -> s != null && !s.isEmpty() && !s.isBlank();

	public static final Predicate<List<?>> NOT_NULL_NOT_EMPTY_LIST = s -> s != null && !s.isEmpty();

	public static final Predicate<Map<?, ?>> NOT_NULL_NOT_EMPTY_MAP = s -> s != null && !s.isEmpty();

	/**
	 * Encoding String in BCryptPasswordEncoder Used for Oauth
	 *
	 * @param  string
	 * @return
	 */
	public static String generateBcrypt(final String string) {
		return new BCryptPasswordEncoder().encode(string);
	}

	/**
	 * Generate random number for OTP
	 *
	 * @return
	 */
	public static int getRandomNumber() {
		final SecureRandom number = new SecureRandom();
		return 100000 + number.nextInt(899999);
	}

	/**
	 * Return Map with new DistinctFileName,fileNameWithOutExtension,extension
	 *
	 * @param  file
	 * @return
	 */
	public static Map<String, String> getDistinctFileProperties(final MultipartFile file) {
		Calendar calendar = Calendar.getInstance();
		long timeInMillis = calendar.getTimeInMillis();
		String assetFile = StringUtils.cleanPath(file.getOriginalFilename());
		String extension = "";
		StringBuilder fileNameWithOutExtension;

		int lastIndex = assetFile.lastIndexOf('.');

		extension = assetFile.substring(lastIndex + 1);
		fileNameWithOutExtension = new StringBuilder(assetFile.substring(0, lastIndex));

		Map<String, String> fileProperties = new HashMap<>();
		StringBuilder newFileNameWithOutExtension = fileNameWithOutExtension.append("_").append(String.valueOf(timeInMillis));
		fileProperties.put("newfileNameWithOutExtension", newFileNameWithOutExtension.toString());
		fileProperties.put("extension", extension);
		StringBuilder newFileName = newFileNameWithOutExtension.append(".").append(extension);
		fileProperties.put("newFileName", newFileName.toString());

		return fileProperties;
	}

	public static String getDistinctFileName(final String fileName) {
		Calendar calendar = Calendar.getInstance();
		long timeInMillis = calendar.getTimeInMillis();
		String extension = "";
		StringBuilder fileNameWithOutExtension;

		int lastIndex = fileName.lastIndexOf('.');

		extension = fileName.substring(lastIndex + 1);
		fileNameWithOutExtension = new StringBuilder(fileName.substring(0, lastIndex));

		StringBuilder newFileName = fileNameWithOutExtension.append("_").append(String.valueOf(timeInMillis));

		return newFileName.append(".").append(extension).toString();
	}

	public static String decode(final String url) {
		try {
			String prevURL = "";
			String decodeURL = url;
			while (!prevURL.equals(decodeURL)) {
				prevURL = decodeURL;
				decodeURL = URLDecoder.decode(decodeURL, "UTF-8");
			}
			return decodeURL;
		} catch (final UnsupportedEncodingException e) {
			return "Issue while decoding" + e.getMessage();
		}
	}

	public static String encode(final String url) {
		try {
			return URLEncoder.encode(url, "UTF-8");
		} catch (final UnsupportedEncodingException e) {
			return "Issue while encoding" + e.getMessage();
		}
	}

	public static LocalDate convetUtilDatetoLocalDate(final Date date) {
		return LocalDateTime.ofInstant(Instant.ofEpochMilli(date.getTime()), ZoneId.systemDefault()).toLocalDate();
	}

	public static Date getDateWithoutTime(final Date date) {
		return Date.from(convetUtilDatetoLocalDate(date).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
	}

	public static Date getTomorrowDateWithoutTime(final Date date) {
		return Date.from(convetUtilDatetoLocalDate(date).plusDays(1).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
	}

	public static Date convertLocalDateToUtilDate(final LocalDate localDate) {
		return Date.from(localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant());
	}

	public static LocalDateTime convertDateToLocalDateTime(final Date dateToConvert) {
		return dateToConvert.toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime();
	}

	public static Double distance(final Double lat1, final Double lon1, final Double lat2, final Double lon2) {
		if (lat1.equals(lat2) && lon1.equals(lon2)) {
			return 0d;
		} else {
			Double theta = lon1 - lon2;
			Double dist = Math.sin(Math.toRadians(lat1)) * Math.sin(Math.toRadians(lat2))
					+ Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.cos(Math.toRadians(theta));
			dist = Math.acos(dist);
			dist = Math.toDegrees(dist);
			dist *= 60 * 1.1515 * 1.609344;
			return round(dist);
		}
	}

	/**
	 * This method returns the 2 decimal value of any given double value
	 *
	 * @param  doubleValue
	 * @return
	 */
	public static Double round(final Double doubleValue) {
		Long orderAmtLong = Math.round(doubleValue * 100);
		return orderAmtLong.doubleValue() / 100;
	}
}
