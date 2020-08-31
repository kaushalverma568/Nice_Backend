/**
 *
 */
package com.nice.service.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.services.s3.AmazonS3;
import com.amazonaws.services.s3.AmazonS3ClientBuilder;
import com.amazonaws.services.s3.model.CannedAccessControlList;
import com.amazonaws.services.s3.model.DeleteObjectRequest;
import com.amazonaws.services.s3.model.PutObjectRequest;
import com.nice.service.AmazonS3ClientService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 18-Jul-2020
 */
@Service(value = "amazonS3ClientService")
@Transactional(rollbackFor = Throwable.class)
public class AmazonS3ClientServiceImpl implements AmazonS3ClientService {

	private String awsS3Bucket;
	private AmazonS3 amazonS3;
	private static final Logger logger = LoggerFactory.getLogger(AmazonS3ClientServiceImpl.class);

	@Autowired
	public AmazonS3ClientServiceImpl(final Region awsRegion, final AWSCredentialsProvider awsCredentialsProvider, final String awsS3Bucket) {
		this.amazonS3 = AmazonS3ClientBuilder.standard().withCredentials(awsCredentialsProvider).withRegion(awsRegion.getName()).build();
		this.awsS3Bucket = awsS3Bucket;
	}

	/**
	 * Upload file to S3 Bucket
	 */
	@Override
	@Async
	public void uploadFileToS3Bucket(final MultipartFile multipartFile, final boolean enablePublicReadAccess, final String newFileName) {
		String fileName = multipartFile.getOriginalFilename();

		/*
		 * creating the file in the server (temporarily)
		 */
		File file = new File(fileName);
		try (FileOutputStream fos = new FileOutputStream(file)) {
			fos.write(multipartFile.getBytes());
			PutObjectRequest putObjectRequest = new PutObjectRequest(this.awsS3Bucket, newFileName, file);

			if (enablePublicReadAccess) {
				putObjectRequest.withCannedAcl(CannedAccessControlList.PublicRead);
			}
			this.amazonS3.putObject(putObjectRequest);
		} catch (IOException | AmazonServiceException ex) {
			logger.error("error {} occurred while uploading {} ", ex.getMessage(), newFileName);
		}
		try {
			/*
			 * removing the file created in the server
			 */
			Files.delete(file.toPath());
		} catch (IOException | AmazonServiceException ex) {
			logger.error("error {} occurred while uploading {} ", ex.getMessage(), newFileName);
		}
	}

	@Override
	@Async
	public void deleteFileFromS3Bucket(final String fileName) {
		try {
			amazonS3.deleteObject(new DeleteObjectRequest(awsS3Bucket, fileName));
		} catch (AmazonServiceException ex) {
			logger.error("error {} occurred while removing {} ", ex.getMessage(), fileName);
		}
	}

	@Override
	public void copyFile(final String sourceFile, final String destinationFile) {
		amazonS3.copyObject(awsS3Bucket, sourceFile, awsS3Bucket, destinationFile);
	}

}
