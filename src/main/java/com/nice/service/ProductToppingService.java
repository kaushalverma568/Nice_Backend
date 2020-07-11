/**
 *
 */
package com.nice.service;

import java.util.List;

import javax.validation.Valid;

import com.nice.dto.ProductToppingDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductTopping;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
public interface ProductToppingService {

	/**
	 * @param productToppingDTOList
	 * @param productVariantId      TODO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateProductTopping(@Valid List<ProductToppingDto> productToppingDTOList, Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductToppingDto getProductTopping(Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param productVariantId
	 * @param active
	 * @return
	 */
	List<ProductToppingDto> getToppingForProductVariant(Long productVariantId, Boolean active);

	/**
	 * @param productToppingId
	 * @param active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long productToppingId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * @param productToppingId
	 * @return
	 * @throws NotFoundException
	 */
	ProductTopping getProductToppingDetails(Long productToppingId) throws NotFoundException;

	/**
	 * @param productToppingList
	 * @return
	 */
	List<ProductToppingDto> convertEntityListToDtos(List<ProductTopping> productToppingList);

	/**
	 * @param productTopping
	 * @return
	 */
	ProductToppingDto convertFromEntityToDto(ProductTopping productTopping);

	/**
	 * @param activeRecords
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductToppingDto> getDtoListWithUserCheck(Boolean activeRecords, Long productVariantId) throws NotFoundException, ValidationException;

}
