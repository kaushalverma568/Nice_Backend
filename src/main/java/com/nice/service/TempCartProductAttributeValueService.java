/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.TempCartProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface TempCartProductAttributeValueService {

	/**
	 * @param tempCartAttributeValueDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addTempCartProductAttributeValue(TempCartProductAttributeValueDTO tempCartAttributeValueDTO, TempCartItem tempCartItem)
			throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAttributeValueDTO> getTempCartProductAttributeValueListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateTempCartProductAttributeValueQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteTempCartProductAttributeValue(Long cartItemId) throws NotFoundException;

}
