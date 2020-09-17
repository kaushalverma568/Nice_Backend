/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.ProductToppingDto;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductTopping;
import com.nice.model.Topping;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
public interface ProductToppingService {

	/**
	 *
	 * @param productToppingDTOList
	 * @param productVariantId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateProductTopping(List<ProductToppingDto> productToppingDTOList, Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductToppingResponseDTO getProductTopping(Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param productVariantId
	 * @param active
	 * @return
	 */
	List<ProductToppingResponseDTO> getToppingForProductVariant(Long productVariantId, Boolean active);

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
	List<ProductToppingResponseDTO> convertEntityListToDtos(List<ProductTopping> productToppingList);

	/**
	 * @param productTopping
	 * @return
	 */
	ProductToppingResponseDTO convertFromEntityToDto(ProductTopping productTopping);

	/**
	 * @param activeRecords
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductToppingResponseDTO> getDtoListWithUserCheck(Boolean activeRecords, Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 * @param topping
	 * @param active
	 * @return
	 */
	List<ProductTopping> getProductToppingListForTopping(Topping topping, Boolean active);

	/**
	 * @param productToppingId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductToppingResponseDTO getProductToppingWithOutUserCheck(Long productToppingId) throws NotFoundException, ValidationException;

}
