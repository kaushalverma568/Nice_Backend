/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.CartToppingsDto;
import com.nice.dto.ProductToppingDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.CartToppings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface CartToppingsService {

	/**
	 * @param tempCartAddonsDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addCartToppings(CartToppingsDto tempCartAddonsDTO, CartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductToppingDto> getProductToppingsDtoListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCartToppingsQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteCartToppings(Long cartItemId) throws NotFoundException;

	/**
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	List<CartToppings> getCartToppingsListForCartItem(Long id) throws NotFoundException;

	/**
	 * @param cartItem
	 * @return
	 */
	List<CartToppings> getCartToppingsListForCartItem(CartItem cartItem);

}
