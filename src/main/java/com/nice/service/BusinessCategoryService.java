package com.nice.service;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.BusinessCategoryDTO;
import com.nice.model.BusinessCategory;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface BusinessCategoryService {

	/**
	 * 
	 * @param businessCategoryDto
	 * @return
	 */
	public boolean isExists(BusinessCategoryDTO businessCategoryDto) ;

	/**
	 * 
	 * @param businessCategoryDto
	 * @param image
	 * @return
	 * @throws NotFoundException
	 */
	public BusinessCategoryDTO addBusinessCategory( BusinessCategoryDTO businessCategoryDto, MultipartFile image) throws NotFoundException ;

	/**
	 * 
	 * @param businessCategoryDTO
	 * @param image
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public BusinessCategoryDTO updateBusinessCategory( BusinessCategoryDTO businessCategoryDTO, MultipartFile image) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @param businessCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	public BusinessCategoryDTO getBusinessCategory(Long businessCategoryId) throws NotFoundException;

	/**
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 */
	public Page<BusinessCategory> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * 
	 * @param businessCategoryId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void changeStatus(Long businessCategoryId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param businessCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	BusinessCategory getBusinessCategoryDetail(Long businessCategoryId) throws NotFoundException;
	
}
