package com.nice.service;

import java.util.List;

import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;

public interface ProductAddonsService {

	boolean isExists(ProductAddonsDTO productAddonsDto, ProductVariant productVariant) throws ValidationException;

	void addUpdateProductAddons(List<ProductAddonsDTO> productAddonsDtoList, Long productVariantId) throws NotFoundException, ValidationException;

	ProductAddonsDTO updateProductAddons(ProductAddonsDTO productAddonsDto) throws NotFoundException, ValidationException;

	ProductAddonsDTO getProductAddons(Long productAddonsId) throws NotFoundException;

	List<ProductAddons> getList(Boolean activeRecords, Long productVariantId) throws NotFoundException;

	void changeStatus(Long productAddonsId, Boolean active) throws ValidationException, NotFoundException;

	ProductAddons getProductAddonsDetail(Long productAddonsId) throws NotFoundException;

	void deleteProductAddons(Long productAddonsId);

	/**
	 * @param activeRecords
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAddonsDTO> getDtoList(Boolean activeRecords, Long productVariantId) throws NotFoundException;

	/**
	 * @param activeRecords
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductAddonsDTO> getDtoListWithUserCheck(Boolean activeRecords, Long productVariantId) throws NotFoundException, ValidationException;

}
