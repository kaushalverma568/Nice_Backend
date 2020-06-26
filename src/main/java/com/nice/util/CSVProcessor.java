package com.nice.util;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.opencsv.CSVParser;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.bean.ColumnPositionMappingStrategy;
import com.opencsv.bean.CsvToBean;
import com.opencsv.bean.StatefulBeanToCsv;
import com.opencsv.bean.StatefulBeanToCsvBuilder;
import com.opencsv.exceptions.CsvDataTypeMismatchException;
import com.opencsv.exceptions.CsvRequiredFieldEmptyException;

/**
 * Common CSV Processor to convert CSV to List of bean and List of bean to CSV
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class CSVProcessor<T> {

	/**
	 * General method for convert CSV to List of bean
	 *
	 * @param  file
	 * @param  clazz
	 * @return
	 * @throws IOException
	 */
	public List<T> convertCSVFileToListOfBean(final File file, final Class<T> clazz) throws IOException {
		final Reader fileReader = new FileReader(file);
		final CSVReader csvReader = new CSVReaderBuilder(fileReader).withSkipLines(1).withCSVParser(new CSVParser()).build();

		final ColumnPositionMappingStrategy<T> columnPositionMappingStrategy = new ColumnPositionMappingStrategy<>();
		columnPositionMappingStrategy.setType(clazz);

		/**
		 * Get field name using Reflection API
		 */
		final List<Field> fieldList = Arrays.asList(clazz.getDeclaredFields());
		final List<String> nonStaticFields = new ArrayList<>();

		for (final Field field : fieldList) {
			if (!java.lang.reflect.Modifier.isStatic(field.getModifiers())) {
				nonStaticFields.add(field.getName());
			}
		}

		/**
		 * Prepare column mapping
		 */
		final String[] columns = nonStaticFields.toArray(new String[nonStaticFields.size()]);
		columnPositionMappingStrategy.setColumnMapping(columns);

		final CsvToBean<T> csvToBean = new CsvToBean<>();
		csvToBean.setCsvReader(csvReader);
		csvToBean.setThrowExceptions(true);
		csvToBean.setMappingStrategy(columnPositionMappingStrategy);
		final List<T> list = csvToBean.parse();

		csvReader.close();
		fileReader.close();
		return list;
	}

	/**
	 * General method for convert List of bean to CSV
	 *
	 * @param  file
	 * @param  allRecords
	 * @throws CsvDataTypeMismatchException
	 * @throws CsvRequiredFieldEmptyException
	 * @throws IOException
	 */
	public void convertListOfBeanToCSVFile(final File file, final List<T> allRecords)
			throws CsvDataTypeMismatchException, CsvRequiredFieldEmptyException, IOException {
		final Writer writer = new FileWriter(file.getPath() + "_result.csv");

		final StatefulBeanToCsvBuilder<T> builder = new StatefulBeanToCsvBuilder<>(writer);
		final StatefulBeanToCsv<T> beanWriter = builder.withSeparator(',').withQuotechar('\"').build();

		beanWriter.write(allRecords);

		writer.close();
	}
}
