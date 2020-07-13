package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.ProductAttributeDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductAttribute;

public interface ProductAttributeService {

	boolean isExists(ProductAttributeDTO productAttributeDto) throws ValidationException;

	ProductAttributeDTO addProductAttribute(ProductAttributeDTO productAttributeDto) throws NotFoundException, ValidationException;

	ProductAttributeDTO updateProductAttribute(ProductAttributeDTO productAttributeDTO) throws NotFoundException, ValidationException;

	ProductAttributeDTO getProductAttribute(Long productAttributeId) throws NotFoundException, ValidationException;

	void changeStatus(Long productAttributeId, Boolean active) throws ValidationException, NotFoundException;

	ProductAttribute getProductAttributeDetail(Long productAttributeId) throws NotFoundException;

	void deleteProductAttribute(Long productAttributeId);

	/**
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 * @throws ValidationException
	 */
	Page<ProductAttribute> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords) throws ValidationException;

	/**
	 * @return
	 * @throws ValidationException
	 */
	List<ProductAttribute> getAllActiveList() throws ValidationException;

}
