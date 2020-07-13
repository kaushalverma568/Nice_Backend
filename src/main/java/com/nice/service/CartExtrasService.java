/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.CartExtrasDto;
import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartExtras;
import com.nice.model.CartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface CartExtrasService {

	/**
	 * @param tempCartExtrasDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addCartExtras(CartExtrasDto tempCartExtrasDTO, CartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtrasDTO> getCartExtrasDtoListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCartExtrasQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteCartExtras(Long cartItemId) throws NotFoundException;

	/**
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	List<CartExtras> getCartExtrasListForCartItem(Long id) throws NotFoundException;

	/**
	 * @param cartItem
	 * @return
	 */
	List<CartExtras> getCartExtrasListForCartItem(CartItem cartItem);

}
