package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.SliderImageDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.SliderImageService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-April-2020
 */
@RequestMapping(path = "/slider")
@RestController
public class SliderImageController {
	private static final Logger LOGGER = LoggerFactory.getLogger(SliderImageController.class);
	public static final String IMAGE_NOT_NULL = "slider.image.required";
	/**
	 * Locale message service - to display response messages from Property file
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SliderImageService sliderBannerService;

	/**
	 * add Slider Banner
	 *
	 * @param accessToken
	 * @param image
	 * @param userId
	 * @param sliderBannerDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 */
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@PostMapping()
	public ResponseEntity<Object> addSliderBanner(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file") final MultipartFile image, @ModelAttribute @Valid final SliderImageDTO sliderBannerDTO, final BindingResult result)
			throws ValidationException {
		LOGGER.info("Inside add Banner {}", sliderBannerDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Banner validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (image == null || !CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage(IMAGE_NOT_NULL, null));
		}
		sliderBannerService.addSliderBanner(sliderBannerDTO, image);
		LOGGER.info("Outside add Banner");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("slider.image.create.message", null))
				.create();
	}

	/**
	 * update Slider Banner
	 *
	 * @param accessToken
	 * @param image
	 * @param userId
	 * @param sliderBannerDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@PutMapping()
	public ResponseEntity<Object> updateSliderBanner(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file") final MultipartFile image, @ModelAttribute @Valid final SliderImageDTO sliderBannerDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Banner {}", sliderBannerDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Banner validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		if (image == null || !CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
			throw new ValidationException(messageByLocaleService.getMessage(IMAGE_NOT_NULL, null));
		}
		sliderBannerService.updateSliderBanner(sliderBannerDTO, image);
		LOGGER.info("Outside update Banner");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("slider.update.message", null))
				.create();
	}

	/**
	 * Get Slider Banner By Id
	 *
	 * @param bannerId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/{bannerId}")
	public ResponseEntity<Object> getSliderBanner(@PathVariable("bannerId") final Long bannerId) throws NotFoundException {
		final SliderImageDTO sliderBannerDTO = sliderBannerService.getSliderBannerDetailById(bannerId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("slider.detail.message", null))
				.setData(sliderBannerDTO).create();
	}

	/**
	 * Get Slider Banner List
	 *
	 * @return
	 */
	@GetMapping("/list")
	public ResponseEntity<Object> getSliderBannerList(@RequestParam(required = false) final String imageType) {
		final List<SliderImageDTO> sliderBannerDTOs = sliderBannerService.getSliderBannerList(imageType);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("slider.list.message", null))
				.setData(sliderBannerDTOs).create();
	}

	@DeleteMapping("/{id}")
	public ResponseEntity<Object> getSliderBannerList(@RequestHeader("Authorization") final String accessToken, @PathVariable final Long id)
			throws NotFoundException, ValidationException {
		sliderBannerService.deleteSliderBannerById(id);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("slider.delete.message", null))
				.create();
	}
}
