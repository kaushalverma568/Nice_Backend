package com.nice.service;

import java.util.List;

import com.nice.dto.ProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductAttribute;
import com.nice.model.ProductAttributeValue;
import com.nice.model.ProductVariant;

public interface ProductAttributeValueService {

	ProductAttributeValueDTO getProductAttributeValue(Long productAttributeValueId) throws NotFoundException;

	List<ProductAttributeValueDTO> getList(Long productVariantId, Boolean activeRecords) throws NotFoundException;

	void changeStatus(Long productAttributeValueId, Boolean active) throws ValidationException, NotFoundException;

	ProductAttributeValue getProductAttributeValueDetail(Long productAttributeValueId) throws NotFoundException;

	void deleteProductAttributeValue(Long productAttributeValueId);

	/**
	 * @param productAttributeValueDTO
	 * @param productVariantId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateProductAttributeValue(List<ProductAttributeValueDTO> productAttributeValueDTO, Long productVariantId)
			throws NotFoundException, ValidationException;

	/**
	 * @param productAttributeValueDTO
	 * @param productVariant
	 * @param productAttribute
	 * @return
	 * @throws NotFoundException
	 */
	boolean isExists(ProductAttributeValueDTO productAttributeValueDTO, ProductVariant productVariant, ProductAttribute productAttribute)
			throws NotFoundException;

}
