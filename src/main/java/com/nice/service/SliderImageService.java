package com.nice.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.SliderImageDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.SliderImage;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
public interface SliderImageService {
	/**
	 * add Slider Banner
	 *
	 * @param sliderBannerDTO
	 * @param appImage
	 * @param webImage
	 * @param userId
	 * @throws ValidationException
	 */
	void addSliderImages(SliderImageDTO sliderBannerDTO, MultipartFile appImage, MultipartFile webImage) throws ValidationException;

	/**
	 * update Slider Banner
	 *
	 * @param sliderBannerDTO
	 * @param appImage
	 * @param webImage
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateSliderImage(SliderImageDTO sliderBannerDTO, MultipartFile appImage, MultipartFile webImage) throws NotFoundException, ValidationException;

	/**
	 * Get Slider Banner DTO List
	 *
	 * @return
	 */
	List<SliderImageDTO> getSliderBannerList(String imageType);

	/**
	 * Slider Banner DTO by id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	SliderImageDTO getSliderBannerDetailById(Long id) throws NotFoundException;

	/**
	 * Slider Banner Entity By Id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	SliderImage getSliderBannerById(Long id) throws NotFoundException;

	/**
	 * @param id
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteSliderBannerById(Long id) throws NotFoundException, ValidationException;
}
