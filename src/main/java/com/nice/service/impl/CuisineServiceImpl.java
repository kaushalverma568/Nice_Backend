package com.nice.service.impl;

import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.dto.CuisineDTO;
import com.nice.dto.CuisineResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CuisineMapper;
import com.nice.model.Cuisine;
import com.nice.model.VendorCuisine;
import com.nice.repository.CuisineRepository;
import com.nice.service.AssetService;
import com.nice.service.CuisineService;
import com.nice.service.VendorCuisineService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Service("cuisineService")
@Transactional(rollbackFor = Throwable.class)
public class CuisineServiceImpl implements CuisineService {

	private static final Logger LOGGER = LoggerFactory.getLogger(CuisineServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CuisineRepository cuisineRepository;

	@Autowired
	private CuisineMapper cuisineMapper;

	@Autowired
	private AssetService assetService;

	@Autowired
	private VendorCuisineService vendorCuisineService;

	@Override
	public void addCuisine(final CuisineDTO cuisineDTO, final MultipartFile image) throws FileOperationException, ValidationException {
		Cuisine cuisine = cuisineMapper.toEntity(cuisineDTO);
		if (image != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
			uploadImage(image, cuisine);
		}
		cuisineRepository.save(cuisine);
	}

	/**
	 * upload image of cuisine
	 *
	 * @param image
	 * @param cuisine
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	private void uploadImage(final MultipartFile image, final Cuisine cuisine) throws FileOperationException, ValidationException {
		cuisine.setImageName(assetService.saveAsset(image, AssetConstant.CUISINE, 0, Constant.CUISINE_IMAGE_WIDTH, Constant.CUISINE_IMAGE_HEIGHT));
		cuisine.setImageOriginalName(image.getOriginalFilename());
	}

	/**
	 * delete old image
	 *
	 * @param cuisine
	 */
	private void deleteOldImage(final Cuisine cuisine) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(cuisine.getImageName())) {
			assetService.deleteFile(cuisine.getImageName(), AssetConstant.CUISINE);
		}
	}

	@Override
	public void updateCuisine(final CuisineDTO cuisineDTO, final MultipartFile image) throws NotFoundException, ValidationException, FileOperationException {
		if (cuisineDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("cuisine.id.not.null", null));
		}
		Cuisine existingCuisine = getCuisineDetails(cuisineDTO.getId());
		Cuisine cuisine = cuisineMapper.toEntity(cuisineDTO);
		/**
		 * delete old image if exist and upload new one
		 */
		if (image != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
			deleteOldImage(existingCuisine);
			uploadImage(image, cuisine);
		} else {
			cuisine.setImageName(existingCuisine.getImageName());
			cuisine.setImageOriginalName(existingCuisine.getImageOriginalName());
		}
		cuisineRepository.save(cuisine);
	}

	@Override
	public CuisineResponseDTO getCuisine(final Long cuisineId) throws NotFoundException {
		return cuisineMapper.toDto(getCuisineDetails(cuisineId));
	}

	@Override
	public Cuisine getCuisineDetails(final Long cuisineId) throws NotFoundException {
		return cuisineRepository.findById(cuisineId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("cuisine.not.found", new Object[] { cuisineId })));
	}

	@Override
	public Page<Cuisine> getCuisineList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyWord)
			throws NotFoundException, ValidationException {
		Pageable pageable;
		Locale locale = LocaleContextHolder.getLocale();
		if (locale.getLanguage().equals("en")) {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameEnglish"));
		} else {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameArabic"));
		}
		if (activeRecords != null) {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(searchKeyWord)) {
				return cuisineRepository.findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(activeRecords,
						searchKeyWord, activeRecords, searchKeyWord, pageable);
			} else {
				return cuisineRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(searchKeyWord)) {
				return cuisineRepository.findAllByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(searchKeyWord, searchKeyWord, pageable);
			} else {
				return cuisineRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long cuisineId, final Boolean active) throws NotFoundException, ValidationException {
		Cuisine existingCuisine = getCuisineDetails(cuisineId);
		LOGGER.info("Existing cuisine details {} ", existingCuisine);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingCuisine.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "cuisine.active" : "cuisine.deactive", null));
		} else {
			if (Boolean.FALSE.equals(active)) {
				/**
				 * deActive vendorCuisine for this cuisine
				 */
				List<VendorCuisine> vendorCuisineList = vendorCuisineService.getVendorCuisineListByCuisine(cuisineId, true);
				for (VendorCuisine vendorCuisine : vendorCuisineList) {
					vendorCuisineService.changeStatus(vendorCuisine.getId(), false);
				}
			}
			existingCuisine.setActive(active);
			cuisineRepository.save(existingCuisine);
		}
	}

	@Override
	public boolean isCuisineExistsEnglish(final CuisineDTO cuisineDTO) {
		if (cuisineDTO.getId() != null) {
			/**
			 * At the time of update is cuisine with same name (English) exist or not
			 */
			return cuisineRepository.findByNameEnglishIgnoreCaseAndIdNot(cuisineDTO.getNameEnglish(), cuisineDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is cuisine with same name (English) exist or not
			 */
			return cuisineRepository.findByNameEnglishIgnoreCase(cuisineDTO.getNameEnglish()).isPresent();
		}
	}

	@Override
	public boolean isCuisineExistsArabic(final CuisineDTO cuisineDTO) {
		if (cuisineDTO.getId() != null) {
			/**
			 * At the time of update is cuisine with same name (Arabic) exist or not
			 */
			return cuisineRepository.findByNameArabicIgnoreCaseAndIdNot(cuisineDTO.getNameArabic(), cuisineDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is cuisine with same name (Arabic) exist or not
			 */
			return cuisineRepository.findByNameArabicIgnoreCase(cuisineDTO.getNameArabic()).isPresent();
		}
	}

	@Override
	public void deleteImage(final Long cuisineId) throws NotFoundException {
		Cuisine cuisine = getCuisineDetails(cuisineId);
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(cuisine.getImageName())) {
			throw new NotFoundException(messageByLocaleService.getMessage("image.not.found", null));
		} else {
			deleteOldImage(cuisine);
			cuisine.setImageName(null);
			cuisine.setImageOriginalName(null);
			cuisineRepository.save(cuisine);
		}
	}

}
