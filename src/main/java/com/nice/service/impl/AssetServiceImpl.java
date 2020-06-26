/**
 *
 */
package com.nice.service.impl;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.nice.service.AssetService;
import com.nice.service.FileStorageService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service(value = "assetService")
public class AssetServiceImpl implements AssetService {

	private static final Logger LOGGER = LoggerFactory.getLogger(AssetServiceImpl.class);

	@Autowired
	private FileStorageService fileStorageService;

	/**
	 * Save images for mentioned sub directory
	 *
	 * @param  image
	 * @param  subDirectory
	 * @param  count        : if value is not 0 then it will append number to imageName to avoid conflict
	 * @return
	 */
	@Override
	public String saveAsset(final MultipartFile image, final String subDirectory, final int count) {
		String newFileName = null;
		if (image != null) {
			final Map<String, String> imageProperties = CommonUtility.getDistinctFileProperties(image);
			if (count != 0) {
				String newfileNameWithOutExtension = imageProperties.remove("newfileNameWithOutExtension");
				String extension = imageProperties.remove("extension");
				newFileName = newfileNameWithOutExtension.concat("_").concat(String.valueOf(count)).concat(".").concat(extension);
			} else {
				newFileName = imageProperties.remove("newFileName");
			}

			fileStorageService.storeFile(image, newFileName, subDirectory);
		}
		LOGGER.info("New file name {} ", newFileName);
		return newFileName;
	}

}
