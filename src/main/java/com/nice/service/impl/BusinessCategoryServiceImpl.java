package com.nice.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.VendorStatus;
import com.nice.dto.BusinessCategoryDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.BusinessCategoryMapper;
import com.nice.model.BusinessCategory;
import com.nice.repository.BusinessCategoryRepository;
import com.nice.service.AssetService;
import com.nice.service.BusinessCategoryService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class BusinessCategoryServiceImpl implements BusinessCategoryService {

	private static final Logger LOGGER = LoggerFactory.getLogger(BusinessCategoryServiceImpl.class);

	@Autowired
	private BusinessCategoryRepository businessCategoryRepository;

	@Autowired
	private BusinessCategoryMapper businessCategoryMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private AssetService assetService;

	@Autowired
	private VendorService vendorService;

	@Override
	public BusinessCategoryDTO addBusinessCategory(final BusinessCategoryDTO businessCategoryDTO, final MultipartFile image) throws NotFoundException {
		BusinessCategory businessCategory = businessCategoryMapper.toEntity(businessCategoryDTO);
		businessCategory.setImageName(assetService.saveAsset(image, AssetConstant.BUSINESS_CATEGORY_DIR, 0));
		businessCategory.setOriginalImageName(image.getOriginalFilename());
		return businessCategoryMapper.toDto(businessCategoryRepository.save(businessCategory));
	}

	@Override
	public BusinessCategoryDTO updateBusinessCategory(final BusinessCategoryDTO businessCategoryDTO, final MultipartFile image)
			throws NotFoundException, ValidationException {
		if (businessCategoryDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("business.category.id.not.null", null));
		}
		BusinessCategory businessCategory = getBusinessCategoryDetail(businessCategoryDTO.getId());
		String oldImageName = businessCategory.getImageName();
		String oldOriginalName = businessCategory.getOriginalImageName();
		if (!businessCategory.getNameEnglish().equals(Constant.BUSINESS_CATEGORY_FOOD_ENGLISH)) {
			businessCategory = businessCategoryMapper.toEntity(businessCategoryDTO);
		}
		if (image != null) {
			assetService.deleteFile(oldImageName, AssetConstant.BUSINESS_CATEGORY_DIR);
			businessCategory.setImageName(assetService.saveAsset(image, AssetConstant.BUSINESS_CATEGORY_DIR, 0));
			businessCategory.setOriginalImageName(image.getOriginalFilename());
		} else {
			businessCategory.setImageName(oldImageName);
			businessCategory.setOriginalImageName(oldOriginalName);
		}
		return businessCategoryMapper.toDto(businessCategoryRepository.save(businessCategory));
	}

	@Override
	public BusinessCategoryDTO getBusinessCategory(final Long businessCategoryId) throws NotFoundException {
		return businessCategoryMapper.toDto(getBusinessCategoryDetail(businessCategoryId));
	}

	@Override
	public void changeStatus(final Long businessCategoryId, final Boolean active) throws ValidationException, NotFoundException {
		BusinessCategory existingBusinessCategory = getBusinessCategoryDetail(businessCategoryId);
		if (existingBusinessCategory.getNameEnglish().equals(Constant.BUSINESS_CATEGORY_FOOD_ENGLISH)) {
			throw new ValidationException(messageByLocaleService.getMessage("food.business.category.not.deactivate", null));
		}
		LOGGER.info("Existing  BusinessCategory details {} ", existingBusinessCategory);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingBusinessCategory.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "business.category.active" : "business.category.deactive", null));
		} else {
			/**
			 * if active vendor contains this business category then can't de-active
			 */
			if (Boolean.FALSE.equals(active)) {
				VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
				vendorFilterDTO.setActiveRecords(true);
				vendorFilterDTO.setBusinessCategoryId(businessCategoryId);
				vendorFilterDTO.setStatus(VendorStatus.ACTIVE.getStatusValue());
				Long vendorCount = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
				if (vendorCount > 0) {
					throw new ValidationException(messageByLocaleService.getMessage("business.category.not.update.vendor", null));
				}
			}
			existingBusinessCategory.setActive(active);
			businessCategoryRepository.save(existingBusinessCategory);
		}
	}

	@Override
	public void updateManageInventory(final Long businessCategoryId, final Boolean manageInventory) throws NotFoundException, ValidationException {
		BusinessCategory existingBusinessCategory = getBusinessCategoryDetail(businessCategoryId);
		if (existingBusinessCategory.getNameEnglish().equals(Constant.BUSINESS_CATEGORY_FOOD_ENGLISH)) {
			throw new ValidationException(messageByLocaleService.getMessage("food.business.category.not.update", null));
		}
		LOGGER.info("Existing  BusinessCategory details {} ", existingBusinessCategory);
		if (manageInventory != null) {
			existingBusinessCategory.setManageInventory(manageInventory);
			businessCategoryRepository.save(existingBusinessCategory);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("manage.inventory.not.null", null));
		}
	}

	@Override
	public Page<BusinessCategory> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			return businessCategoryRepository.findAllByActive(activeRecords, pageable);
		} else {
			return businessCategoryRepository.findAll(pageable);
		}
	}

	@Override
	public boolean isExistsEnglish(final BusinessCategoryDTO businessCategoryDTO) {
		if (businessCategoryDTO.getId() != null) {
			return !(businessCategoryRepository.findByNameEnglishIgnoreCaseAndIdNot(businessCategoryDTO.getNameEnglish(), businessCategoryDTO.getId())
					.isEmpty());
		} else {
			return !(businessCategoryRepository.findByNameEnglishIgnoreCase(businessCategoryDTO.getNameEnglish()).isEmpty());
		}
	}

	@Override
	public boolean isExistsArabic(final BusinessCategoryDTO businessCategoryDTO) {
		if (businessCategoryDTO.getId() != null) {
			return !(businessCategoryRepository.findByNameArabicIgnoreCaseAndIdNot(businessCategoryDTO.getNameArabic(), businessCategoryDTO.getId()).isEmpty());
		} else {
			return !(businessCategoryRepository.findByNameArabicIgnoreCase(businessCategoryDTO.getNameArabic()).isEmpty());
		}
	}

	@Override
	public BusinessCategory getBusinessCategoryDetail(final Long BusinessCategoryId) throws NotFoundException {
		return businessCategoryRepository.findById(BusinessCategoryId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("business.category.not.found", new Object[] { BusinessCategoryId })));
	}

}
