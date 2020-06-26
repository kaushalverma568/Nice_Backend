package com.nice.service;

import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileStorageException;
import com.nice.util.FileStorageProperties;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service("fileStorageService")
public class FileStorageService {

	private final Path fileStorageLocation;
	private static final Logger LOGGER = LoggerFactory.getLogger(FileStorageService.class);

	/**
	 * Set property files
	 *
	 * @param fileStorageProperties
	 */
	@Autowired
	public FileStorageService(final FileStorageProperties fileStorageProperties) {
		this.fileStorageLocation = Paths.get(fileStorageProperties.getUploadDir()).toAbsolutePath().normalize();

		/**
		 * Create Directory if it is not exists
		 */
		try {
			Files.createDirectories(this.fileStorageLocation);
		} catch (Exception ex) {
			throw new FileStorageException("Could not create the directory where the uploaded files will be stored.", ex);
		}
	}

	/**
	 * Stored file as new fileName which is passed as argument on mentioned sub directory
	 *
	 * @param  file
	 * @param  newFileName
	 * @param  subDir
	 * @return
	 */
	public String storeFile(final MultipartFile file, final String newFileName, final String subDir) {
		/**
		 * Normalize file name
		 */
		String fileName = StringUtils.cleanPath(file.getOriginalFilename());

		try {
			/**
			 * Check if the file's name contains invalid characters
			 */
			if (fileName.contains("..")) {
				throw new FileStorageException("Sorry! Filename contains invalid path sequence " + fileName);
			}
			StringBuilder finalPath = new StringBuilder(this.fileStorageLocation.toString()).append(subDir);
			/**
			 * Sub directory is created if it is not exists
			 */
			if (!Files.exists(Paths.get(finalPath.toString()))) {
				Files.createDirectories(Paths.get(finalPath.toString()));
			}
			Path finalPathLocaton = Paths.get(finalPath.toString()).resolve(newFileName);
			/**
			 * Copy file to the target location (Replacing existing file with the same name)
			 */
			Files.copy(file.getInputStream(), finalPathLocaton, StandardCopyOption.REPLACE_EXISTING);

			return finalPathLocaton.toString();
		} catch (IOException ex) {
			throw new FileStorageException("Could not store file " + fileName + ". Please try again!", ex);
		}
	}

	/**
	 * Retrieve file based on fileName and sub directory
	 *
	 * @param  fileName
	 * @param  subDir
	 * @return
	 * @throws FileNotFoundException
	 */
	public Resource loadFileAsResource(final String fileName, final String subDir) throws FileNotFoundException {
		try {
			StringBuilder finalPath = new StringBuilder(this.fileStorageLocation.toString()).append(subDir);
			Path finalPathLocaton = Paths.get(finalPath.toString()).resolve(fileName);
			Resource resource = new UrlResource(finalPathLocaton.toUri());
			if (resource.exists()) {
				return resource;
			} else {
				throw new FileNotFoundException("File not found " + fileName);
			}
		} catch (MalformedURLException ex) {
			throw new FileNotFoundException("File not found " + fileName, ex);
		}
	}

	/**
	 * Delete file on its location
	 *
	 * @param fileName
	 * @param subDir
	 */
	public void deleteFile(final String fileName, final String subDir) {
		StringBuilder finalPath = new StringBuilder(this.fileStorageLocation.toString()).append(subDir);
		Path targetLocation = Paths.get(finalPath.toString()).resolve(fileName);
		LOGGER.info("location: {}", targetLocation);
		try {
			Files.delete(targetLocation);
		} catch (IOException e) {
			LOGGER.info("Exception throws from FileStorageService:deleteImagefile() with cause {}:  and detail message : {}", e.getCause(), e.getMessage());
		}

	}

	public void deleteAllFileFromFolder(final String subDir) {
		StringBuilder finalPath = new StringBuilder(this.fileStorageLocation.toString()).append(subDir);
		try (Stream<Path> walk = Files.walk(Paths.get(finalPath.toString()))) {
			List<Path> result = walk.collect(Collectors.toList());
			for (int i = 1; i < result.size(); i++) {
				BasicFileAttributes attrs = Files.readAttributes(result.get(i), BasicFileAttributes.class);
				LocalDateTime fileLocalDateTime = LocalDateTime.ofInstant((attrs.creationTime()).toInstant(), ZoneId.systemDefault());
				if ((fileLocalDateTime.toLocalDate()).isBefore(LocalDate.now())) {
					Files.delete(result.get(i));
				}
			}
		} catch (IOException e) {
			LOGGER.info("Exception throws from FileStorageService: with cause {}:  and detail message : {}", e.getCause(), e.getMessage());
		}
	}

	/**
	 * Get original file
	 *
	 * @param  fileName
	 * @param  subDir
	 * @return
	 */
	public Path getOriginalFilePath(final String fileName, final String subDir) {
		StringBuilder finalPath = new StringBuilder(this.fileStorageLocation.toString()).append(subDir);
		try {
			/**
			 * Sub directory is created if it is not exists
			 */
			if (!Files.exists(Paths.get(finalPath.toString()))) {
				Files.createDirectories(Paths.get(finalPath.toString()));
			}
		} catch (IOException ex) {
			throw new FileStorageException("Could not store file " + fileName + ". Please try again!", ex);
		}
		return Paths.get(finalPath.toString()).resolve(fileName);
	}
}
