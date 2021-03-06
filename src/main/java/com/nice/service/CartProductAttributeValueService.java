/**
 *
 */
package com.nice.service;

import java.util.List;

import com.nice.dto.CartProductAttributeValueDTO;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.CartProductAttributeValue;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
public interface CartProductAttributeValueService {

	/**
	 * @param tempCartAttributeValueDTO
	 * @param tempCartItem
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addCartProductAttributeValue(CartProductAttributeValueDTO tempCartAttributeValueDTO, CartItem tempCartItem)
			throws ValidationException, NotFoundException;

	/**
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductAttributeValueDTO> getProductAttributeValueDtoListForCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCartProductAttributeValueQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteCartProductAttributeValue(Long cartItemId) throws NotFoundException;

	/**
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(Long id) throws NotFoundException;

	/**
	 *
	 * @param cartItem
	 * @return
	 */
	List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(CartItem cartItem);

}
