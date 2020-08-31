/**
 *
 */
package com.nice.service.impl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.exception.FileStorageException;
import com.nice.service.AmazonS3ClientService;
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

	@Autowired
	private AmazonS3ClientService amazonS3ClientService;

	@Value("${service.url}")
	private String serviceUrl;

	@Value("${s3.url}")
	private String s3Url;

	@Override
	public String saveAsset(final MultipartFile image, final String subDirectory, final int count) {
		if (AssetConstant.IS_AWS) {
			return saveAssetAws(image, subDirectory, count);
		} else {
			return saveAssetLocal(image, subDirectory, count);
		}
	}

	/**
	 * Save images for mentioned sub directory
	 *
	 * @param  image
	 * @param  subDirectory
	 * @param  count        : if value is not 0 then it will append number to imageName to avoid conflict
	 * @return
	 */
	private String saveAssetLocal(final MultipartFile image, final String subDirectory, final int count) {
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

	private String saveAssetAws(final MultipartFile image, final String subDirectory, final int count) {
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
			amazonS3ClientService.uploadFileToS3Bucket(image, true, subDirectory.concat("/").concat(newFileName));
		}
		LOGGER.info("New file name {} ", newFileName);
		return newFileName;
	}

	@Override
	public void deleteFile(final String fileName, final String direcotry) {
		if (AssetConstant.IS_AWS) {
			deleteFileAws(fileName, direcotry);
		} else {
			deleteFileLocal(fileName, direcotry);
		}
	}

	private void deleteFileLocal(final String fileName, final String direcotry) {
		fileStorageService.deleteFile(fileName, direcotry);
	}

	private void deleteFileAws(final String fileName, final String direcotry) {
		amazonS3ClientService.deleteFileFromS3Bucket(direcotry.concat("/").concat(fileName));
	}

	@Override
	public String getGeneratedUrl(final String newFileName, final String subDirPath) {
		if (AssetConstant.IS_AWS) {
			return getGeneratedUrlAws(newFileName, subDirPath);
		} else {
			return getGeneratedUrlLocal(newFileName, subDirPath);
		}
	}

	private String getGeneratedUrlLocal(final String newFileName, final String subDirPath) {
		if (newFileName != null) {
			final StringBuilder imgUrl = new StringBuilder();
			imgUrl.append(serviceUrl).append(AssetConstant.DIGITAL_ASSET_DOWNLOAD).append(newFileName);
			if (subDirPath != null) {
				imgUrl.append("?subDir=/" + subDirPath);
			}
			return imgUrl.toString();
		}
		return null;
	}

	private String getGeneratedUrlAws(final String newFileName, final String subDirPath) {
		if (newFileName != null) {
			final StringBuilder imgUrl = new StringBuilder(s3Url);
			imgUrl.append(subDirPath).append("/").append(newFileName);
			return imgUrl.toString();
		} else {
			return null;
		}
	}

	@Override
	public void moveFiles(final String fileName, final String sourceDirectory, final String destinationDirctory) {
		if (AssetConstant.IS_AWS) {
			amazonS3ClientService.copyFile(sourceDirectory.concat("/").concat(fileName), destinationDirctory.concat("/").concat(fileName));
			deleteFileAws(fileName, sourceDirectory);
		} else {

			Path backSource = fileStorageService.getOriginalFilePath(fileName, sourceDirectory);
			Path backDestination = fileStorageService.getOriginalFilePath(fileName, destinationDirctory);
			try {
				Files.move(backSource, backDestination, StandardCopyOption.REPLACE_EXISTING);
			} catch (final IOException e) {
				throw new FileStorageException("Error while moving file " + fileName + ". Please try again!", e);
			}
		}
	}

}
