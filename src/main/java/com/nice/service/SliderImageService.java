package com.nice.service;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.SliderImageDTO;
import com.nice.dto.SliderImageResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.SliderImage;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface SliderImageService {
	/**
	 * add Slider Banner
	 *
	 * @param  sliderBannerDTO
	 * @param  imageEnglish
	 * @param  imageArabic
	 * @param  userId
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void addSliderImages(SliderImageDTO sliderBannerDTO, MultipartFile imageEnglish, MultipartFile imageArabic)
			throws ValidationException, FileOperationException;

	/**
	 * Update Slider Banner
	 *
	 * @param  sliderBannerDTO
	 * @param  imageEnglish
	 * @param  imageArabic
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void updateSliderImage(SliderImageDTO sliderBannerDTO, MultipartFile imageEnglish, MultipartFile imageArabic)
			throws NotFoundException, ValidationException, FileOperationException;

	/**
	 * Get Slider Banner DTO List
	 *
	 * @return
	 */
	List<SliderImageResponseDTO> getSliderBannerList(String imageType);

	/**
	 * Slider Banner DTO by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	SliderImageResponseDTO getSliderBannerDetailById(Long id) throws NotFoundException;

	/**
	 * Slider Banner Entity By Id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	SliderImage getSliderBannerById(Long id) throws NotFoundException;

	/**
	 * @param  id
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteSliderBannerById(Long id) throws NotFoundException, ValidationException;
}
