package com.nice.service;

import java.util.List;

import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Addons;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;

public interface ProductAddonsService {

	/**
	 *
	 * @param productAddonsDto
	 * @param productVariant
	 * @param addons
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	boolean isExists(ProductAddonsDTO productAddonsDto, ProductVariant productVariant, Addons addons) throws ValidationException, NotFoundException;

	/**
	 *
	 * @param productAddonsDtoList
	 * @param productVariantId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateProductAddons(List<ProductAddonsDTO> productAddonsDtoList, Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param productAddonsId
	 * @return
	 * @throws NotFoundException
	 */
	ProductAddonsDTO getProductAddons(Long productAddonsId) throws NotFoundException;

	/**
	 *
	 * @param activeRecords
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAddons> getList(Boolean activeRecords, Long productVariantId) throws NotFoundException;

	/**
	 *
	 * @param productAddonsId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long productAddonsId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 *
	 * @param productAddonsId
	 * @return
	 * @throws NotFoundException
	 */
	ProductAddons getProductAddonsDetail(Long productAddonsId) throws NotFoundException;

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

	/**
	 * @param addonsId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAddons> getListByAddonsId(Long addonsId) throws NotFoundException;

}
