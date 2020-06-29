package com.nice.service;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.BusinessCategoryDTO;
import com.nice.model.BusinessCategory;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface BusinessCategoryService {

	public boolean isExists(BusinessCategoryDTO businessCategoryDto) ;

	public BusinessCategoryDTO addBusinessCategory( BusinessCategoryDTO businessCategoryDto, MultipartFile image) throws NotFoundException ;

	public BusinessCategoryDTO updateBusinessCategory( BusinessCategoryDTO businessCategoryDTO, MultipartFile image) throws NotFoundException, ValidationException;

	public BusinessCategoryDTO getBusinessCategory(Long businessCategoryId) throws NotFoundException;

	public Page<BusinessCategory> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	public void changeStatus(Long businessCategoryId, Boolean active) throws ValidationException, NotFoundException;

	BusinessCategory getBusinessCategoryDetail(Long businessCategoryId) throws NotFoundException;
	
}
