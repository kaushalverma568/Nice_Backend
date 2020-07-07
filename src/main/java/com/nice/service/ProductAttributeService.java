package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.ProductAttributeDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductAttribute;

public interface ProductAttributeService {

	boolean isExists(ProductAttributeDTO productAttributeDto) throws ValidationException;

	ProductAttributeDTO addProductAttribute(ProductAttributeDTO productAttributeDto) throws NotFoundException;

	ProductAttributeDTO updateProductAttribute(ProductAttributeDTO productAttributeDTO) throws NotFoundException, ValidationException;

	ProductAttributeDTO getProductAttribute(Long productAttributeId) throws NotFoundException;

	Page<ProductAttribute> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Long vendorId);

	void changeStatus(Long productAttributeId, Boolean active) throws ValidationException, NotFoundException;

	ProductAttribute getProductAttributeDetail(Long productAttributeId) throws NotFoundException;

	void deleteProductAttribute(Long productAttributeId);

}
