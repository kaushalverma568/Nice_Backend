/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.CartAddonsDTO;
import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartAddons;
import com.nice.model.CartItem;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 05-Jul-2020
 */
public interface CartAddonsService {

	/**
	 * @param  tempCartAddonsDTO
	 * @param  tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addCartAddons(CartAddonsDTO tempCartAddonsDTO, CartItem tempCartItem) throws ValidationException, NotFoundException;

	/**
	 * @param  cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAddonsDTO> getCartAddonsDtoListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param  cartItemId
	 * @param  quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCartAddonsQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param  cartItemId
	 * @throws NotFoundException
	 */
	void deleteCartAddons(Long cartItemId) throws NotFoundException;

	/**
	 * @param  cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<CartAddons> getCartAddonsListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param  cartItem
	 * @return
	 */
	List<CartAddons> getCartAddonsListForCartItem(CartItem cartItem);

	/**
	 * get cart addons list based on product addons
	 *
	 * @param  productAddonsId
	 * @return
	 * @throws NotFoundException
	 */
	List<CartAddons> getCartAddonsByProductAddOns(Long productAddonsId) throws NotFoundException;

	/**
	 * @param id
	 */
	void deleteByCartAddonsId(Long id);

}
