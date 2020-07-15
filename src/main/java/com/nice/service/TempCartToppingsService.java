/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.ProductToppingResponseDTO;
import com.nice.dto.TempCartToppingsDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface TempCartToppingsService {

	/**
	 * @param tempCartAddonsDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addTempCartToppings(TempCartToppingsDto tempCartAddonsDTO, TempCartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductToppingResponseDTO> getTempCartToppingsListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateTempCartToppingsQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteTempCartToppings(Long cartItemId) throws NotFoundException;

}
