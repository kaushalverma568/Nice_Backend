/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.ProductAddonsDTO;
import com.nice.dto.TempCartAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface TempCartAddonsService {

	/**
	 * @param tempCartAddonsDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addTempCartAddons(TempCartAddonsDTO tempCartAddonsDTO, TempCartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAddonsDTO> getTempCartAddonsListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateTempCartAddonsQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteTempCartAddons(Long cartItemId) throws NotFoundException;

}
