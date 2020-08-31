package com.nice.service;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.BusinessCategoryDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.BusinessCategory;

public interface BusinessCategoryService {

	/**
	 *
	 * @param businessCategoryDto
	 * @return
	 */
	boolean isExists(BusinessCategoryDTO businessCategoryDto);

	/**
	 *
	 * @param businessCategoryDto
	 * @param image
	 * @return
	 * @throws NotFoundException
	 */
	BusinessCategoryDTO addBusinessCategory(BusinessCategoryDTO businessCategoryDto, MultipartFile image) throws NotFoundException;

	/**
	 *
	 * @param businessCategoryDTO
	 * @param image
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	BusinessCategoryDTO updateBusinessCategory(BusinessCategoryDTO businessCategoryDTO, MultipartFile image) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param businessCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	BusinessCategoryDTO getBusinessCategory(Long businessCategoryId) throws NotFoundException;

	/**
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 */
	Page<BusinessCategory> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 *
	 * @param businessCategoryId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long businessCategoryId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 *
	 * @param businessCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	BusinessCategory getBusinessCategoryDetail(Long businessCategoryId) throws NotFoundException;

	/**
	 *
	 * @param businessCategoryId
	 * @param manageInventory
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateManageInventory(Long businessCategoryId, Boolean manageInventory) throws NotFoundException, ValidationException;

}
