/**
 *
 */
package com.nice.service;

import org.springframework.web.multipart.MultipartFile;

import com.nice.exception.FileOperationException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public interface AssetService {

	/**
	 * @param image
	 * @param subDirectory
	 * @param count
	 * @return
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	String saveAsset(MultipartFile image, String subDirectory, int count, int width, int height) throws FileOperationException, ValidationException;

	/**
	 * @param fileName
	 * @param direcotry
	 */
	void deleteFile(String fileName, String direcotry);

	/**
	 * @param newFileName
	 * @param subDirPath
	 * @return
	 */
	String getGeneratedUrl(String newFileName, String subDirPath);

	/**
	 * @param fileName
	 * @param sourceDirectory
	 * @param destinationDirctory
	 */
	void moveFiles(final String fileName, final String sourceDirectory, final String destinationDirctory);

}
