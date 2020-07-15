/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.ProductExtrasDTO;
import com.nice.dto.TempCartExtrasDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.TempCartExtras;
import com.nice.model.TempCartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface TempCartExtrasService {

	/**
	 * @param tempCartExtrasDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addTempCartExtras(TempCartExtrasDto tempCartExtrasDTO, TempCartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtrasDTO> getTempCartExtrasListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateTempCartExtrasQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteTempCartExtras(Long cartItemId) throws NotFoundException;

	/**
	 * get list by productExtrasId
	 * 
	 * @param productExtrasId
	 * @return
	 * @throws NotFoundException
	 */
	List<TempCartExtras> getTempCartExtrasListFromProductExtrasId(Long productExtrasId) throws NotFoundException;

	/**
	 * 
	 * @param productExtrasId
	 * @throws NotFoundException
	 */
	void deleteTempCartExtrasByExtrasId(Long productExtrasId) throws NotFoundException;

}
