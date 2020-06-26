package com.nice.util;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.io.ICsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@PropertySource("classpath:export.properties")
@Service("exportCSV")
public class ExportCSV {
	@Value("${export.content.disposition}")
	private String exportContentDisposition;
	@Value("${export.csv.content.type}")
	private String exportCsvContentType;
	@Value("${export.csv.file.attachment}")
	private String exportCsvFileAttachment;
	@Value("${export.csv.file.name}")
	private String exportCsvFileName;

	private final Logger logger = LoggerFactory.getLogger(this.getClass());

	/**
	 * Write CSV file based on list
	 *
	 * @param  objList
	 * @param  fields
	 * @param  httpServletResponse
	 * @throws IOException
	 */
	public void writeCSVFile(final List<?> objList, final Object[] fields, final HttpServletResponse httpServletResponse) throws IOException {
		httpServletResponse.setContentType(exportCsvContentType);
		String headerValue = String.format(exportCsvFileAttachment, exportCsvFileName);
		httpServletResponse.setHeader(exportContentDisposition, headerValue);
		String[] header = Arrays.copyOf(fields, fields.length, String[].class);
		try (ICsvBeanWriter csvWriter = new CsvBeanWriter(httpServletResponse.getWriter(), CsvPreference.STANDARD_PREFERENCE)) {
			csvWriter.writeHeader(header);
			for (Object obj : objList) {
				csvWriter.write(obj, header);
			}
		} catch (IOException e) {
			logger.info(e.getCause().getMessage());
		}

	}

	/**
	 * Used for convert list to CSV based on fields and field header name
	 *
	 * @param  objList
	 * @param  fields
	 * @param  headerField
	 * @param  httpServletResponse
	 * @throws IOException
	 */
	public void writeCSVFile(final List<?> objList, final Object[] fields, final Object[] headerField, final HttpServletResponse httpServletResponse)
			throws IOException {
		httpServletResponse.setContentType(exportCsvContentType);
		String headerValue = String.format(exportCsvFileAttachment, exportCsvFileName);
		httpServletResponse.setHeader(exportContentDisposition, headerValue);
		String[] header = Arrays.copyOf(fields, fields.length, String[].class);
		String[] displayHeader = Arrays.copyOf(headerField, headerField.length, String[].class);

		try (ICsvBeanWriter csvWriter = new CsvBeanWriter(httpServletResponse.getWriter(), CsvPreference.STANDARD_PREFERENCE)) {
			csvWriter.writeHeader(displayHeader);
			for (Object obj : objList) {
				csvWriter.write(obj, header);
			}
		} catch (IOException e) {
			logger.info(e.getCause().getMessage());
		}
	}
}
