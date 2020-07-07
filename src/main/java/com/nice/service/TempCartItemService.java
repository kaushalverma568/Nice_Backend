package com.nice.service;

import java.util.List;

import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.TempCartItemDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductVariant;
import com.nice.model.TempCartItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
public interface TempCartItemService {
	/**
	 * add cart item
	 *
	 * @param tempTempCartItemDTO
	 * @param userId
	 * @param wishListItem
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Long addTempCartItem(final TempCartItemDTO tempCartItemDTO) throws ValidationException, NotFoundException;

	/**
	 * get cart item by user login id , product id and product variant id
	 *
	 * @param uuid
	 * @param productId
	 * @param productVariantId
	 * @return
	 */
	List<TempCartItem> getTempCartItemBasedOnUuidAndProductVariant(String uuid, ProductVariant productVariant);

	/**
	 * get Cart Item Details
	 *
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	TempCartItem getTempCartItemDetail(Long cartItemId) throws NotFoundException;

	/**
	 * add multiple cart item
	 *
	 * @param tempTempCartItemDTO s
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addTempCartItemList(List<TempCartItemDTO> tempTempCartItemDTOs) throws NotFoundException, ValidationException;

	/**
	 * delete cart item
	 *
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteTempCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * get cart item count based on customer
	 *
	 * @param uuid
	 * @return
	 * @throws ValidationException
	 */
	Long getTempCartItemCountForUuid(String uuid) throws ValidationException;

	/**
	 * get cart item list for user Login
	 *
	 * @param uuid
	 * @return
	 * @throws ValidationException
	 */
	List<TempCartItem> getCartListBasedOnUuid(String uuid) throws ValidationException;

	/**
	 * cart list for customer with extra field in response
	 *
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<CartItemResponseDTO> getTempCartItemDetailListByParam(String uuid) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateTempCartItemQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param uuid
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteTempCartItemForUuid(String uuid) throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @param pincodeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	CartItemResponseDTO getTempCartItem(Long cartItemId, Long pincodeId) throws NotFoundException, ValidationException;

	/**
	 * @param productVariantId
	 * @throws NotFoundException
	 */
	void deleteCartItemsForProductVariant(Long productVariantId) throws NotFoundException;
}
