package com.nice.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.dto.SliderImageDTO;
import com.nice.dto.SliderImageResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SliderImageMapper;
import com.nice.model.SliderImage;
import com.nice.repository.SliderImageRepository;
import com.nice.service.AssetService;
import com.nice.service.SliderImageService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service("sliderImageService")
@Transactional(rollbackFor = Throwable.class)
public class SliderImageServiceImpl implements SliderImageService {

	@Autowired
	private SliderImageRepository sliderImageRepository;

	@Autowired
	private AssetService assetService;

	@Autowired
	private SliderImageMapper sliderBannerMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addSliderImages(final SliderImageDTO sliderImageDTO, final MultipartFile imageEnglish, final MultipartFile imageArabic)
			throws ValidationException {
		if (Constant.BANNER.equalsIgnoreCase(sliderImageDTO.getType())
				&& sliderImageRepository.findAllByType(Constant.BANNER).size() == Constant.MAX_BANNER_IMAGES) {
			throw new ValidationException(messageByLocaleService.getMessage("banner.slider.image.limit.exaust", new Object[] { Constant.MAX_BANNER_IMAGES }));
		}
		SliderImage sliderBanner = sliderBannerMapper.toEntity(sliderImageDTO);
		sliderBanner.setImageNameEnglish(assetService.saveAsset(imageEnglish, AssetConstant.SLIDER_IMAGES, 0));
		sliderBanner.setImageOrigionalNameEnglish(imageEnglish.getOriginalFilename());
		sliderBanner.setImageNameArabic(assetService.saveAsset(imageArabic, AssetConstant.SLIDER_IMAGES, 1));
		sliderBanner.setImageOrigionalNameArabic(imageArabic.getOriginalFilename());
		sliderImageRepository.save(sliderBanner);

	}

	@Override
	public void updateSliderImage(final SliderImageDTO sliderImageDTO, final MultipartFile imageEnglish, final MultipartFile imageArabic)
			throws NotFoundException, ValidationException {
		if (sliderImageDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("slider.image.id.not.null", null));
		}
		SliderImage existingSliderBanner = getSliderBannerById(sliderImageDTO.getId());
		if (!existingSliderBanner.getType().equals(sliderImageDTO.getType())) {
			throw new ValidationException(messageByLocaleService.getMessage("slider.image.type.cannot.change", null));
		}
		SliderImage sliderBanner = sliderBannerMapper.toEntity(sliderImageDTO);

		if (imageEnglish != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(imageEnglish.getOriginalFilename())) {
			assetService.deleteFile(existingSliderBanner.getImageNameEnglish(), AssetConstant.SLIDER_IMAGES);
			sliderBanner.setImageNameEnglish(assetService.saveAsset(imageEnglish, AssetConstant.SLIDER_IMAGES, 0));
			sliderBanner.setImageOrigionalNameEnglish(imageEnglish.getOriginalFilename());
		} else {
			sliderBanner.setImageNameEnglish(existingSliderBanner.getImageNameEnglish());
			sliderBanner.setImageOrigionalNameEnglish(existingSliderBanner.getImageOrigionalNameEnglish());
		}
		if (imageArabic != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(imageArabic.getOriginalFilename())) {
			assetService.deleteFile(existingSliderBanner.getImageNameArabic(), AssetConstant.SLIDER_IMAGES);
			sliderBanner.setImageNameArabic(assetService.saveAsset(imageArabic, AssetConstant.SLIDER_IMAGES, 1));
			sliderBanner.setImageOrigionalNameArabic(imageArabic.getOriginalFilename());
		} else {
			sliderBanner.setImageNameArabic(existingSliderBanner.getImageNameArabic());
			sliderBanner.setImageOrigionalNameArabic(existingSliderBanner.getImageOrigionalNameArabic());
		}
		sliderImageRepository.save(sliderBanner);
	}

	@Override
	public List<SliderImageResponseDTO> getSliderBannerList(final String imageType) {
		List<SliderImage> sliderBanners = null;
		if (imageType != null) {
			sliderBanners = sliderImageRepository.findAllByType(imageType);
		} else {
			sliderBanners = sliderImageRepository.findAll();
		}
		return sliderBannerMapper.toDtos(sliderBanners);
	}

	@Override
	public SliderImage getSliderBannerById(final Long id) throws NotFoundException {
		return sliderImageRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("slider.image.not.exists", new Object[] { id })));
	}

	@Override
	public SliderImageResponseDTO getSliderBannerDetailById(final Long id) throws NotFoundException {
		return sliderBannerMapper.toDto(getSliderBannerById(id));
	}

	@Override
	public void deleteSliderBannerById(final Long id) throws NotFoundException, ValidationException {
		if (id == null) {
			throw new ValidationException(messageByLocaleService.getMessage("slider.image.id.not.null", null));
		}
		SliderImage existingSliderBanner = getSliderBannerById(id);
		assetService.deleteFile(existingSliderBanner.getImageNameEnglish(), AssetConstant.SLIDER_IMAGES);
		assetService.deleteFile(existingSliderBanner.getImageNameArabic(), AssetConstant.SLIDER_IMAGES);
		sliderImageRepository.delete(existingSliderBanner);

	}
}
