/**
 *
 */
package com.nice.util;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.imageio.ImageIO;

import org.apache.http.entity.ContentType;
import org.imgscalr.Scalr;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.Constant;
import com.nice.exception.FileOperationException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Aug-2020
 */
@Component
public class ImageUtility {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(ImageUtility.class);

	public MultipartFile resizeImage(final MultipartFile originalImage) throws FileOperationException, ValidationException {
		try {
			InputStream inputStream = originalImage.getInputStream();
			BufferedImage img = ImageIO.read(inputStream);
			inputStream.close();
			ByteArrayOutputStream thumbOutput = new ByteArrayOutputStream();
			String extension = originalImage.getOriginalFilename().substring(originalImage.getOriginalFilename().lastIndexOf(".") + 1,
					originalImage.getOriginalFilename().length());
			if (img == null) {
				throw new ValidationException(messageByLocaleService.getMessage("images.type", null));
			}
			int targetWidth = Constant.IMAGE_WIDTH;
			int targetHeight = Constant.IMAGE_HEIGHT;
			img = Scalr.resize(img, Scalr.Method.AUTOMATIC, Scalr.Mode.FIT_EXACT, targetWidth, targetHeight);

			ImageIO.write(img, extension, thumbOutput);
			thumbOutput.flush();
			byte[] data = thumbOutput.toByteArray();
			thumbOutput.close();
			InputStream inputStreamNewImage = new ByteArrayInputStream(data);

			return new MockMultipartFile(originalImage.getOriginalFilename(), originalImage.getOriginalFilename(),
					ContentType.MULTIPART_FORM_DATA.getMimeType(), inputStreamNewImage);
		} catch (IOException e) {
			LOGGER.error("IO Exception in Image processing : {0}", e);
			throw new FileOperationException(e.getMessage());
		}
	}
}
