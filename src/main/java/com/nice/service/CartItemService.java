package com.nice.service;

import java.util.List;

import com.nice.dto.CartItemDTO;
import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.Customer;
import com.nice.model.ProductVariant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
public interface CartItemService {
	/**
	 * add cart item
	 *
	 * @param tempCartItemDTO
	 * @param userId
	 * @param wishListItem
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<CartItemResponseDTO> addCartItem(final CartItemDTO tempCartItemDTO) throws ValidationException, NotFoundException;

	/**
	 * get cart item by user login id , product id and product variant id
	 *
	 * @param uuid
	 * @param productId
	 * @param productVariantId
	 * @return
	 */
	List<CartItem> getCartItemBasedOnCustomerAndProductVariant(Customer customer, ProductVariant productVariant);

	/**
	 * get Cart Item Details
	 *
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 */
	CartItem getCartItemDetail(Long cartItemId) throws NotFoundException;

	/**
	 * add multiple cart item
	 *
	 * @param tempCartItemDTO s
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addCartItemList(List<CartItemDTO> tempCartItemDTOs) throws NotFoundException, ValidationException;

	/**
	 * delete cart item
	 *
	 * @param cartItemId
	 * @throws NotFoundException
	 */
	void deleteCartItem(Long cartItemId) throws NotFoundException;

	/**
	 * cart list for customer with extra field in response
	 *
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<CartItemResponseDTO> getCartItemDetailList() throws NotFoundException, ValidationException;

	/**
	 * @param cartItemId
	 * @param quantity
	 * @param userId
	 * @return TODO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<CartItemResponseDTO> updateCartItemQty(Long cartItemId, Long quantity) throws NotFoundException, ValidationException;

	/**
	 * @param customerId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<CartItem> getCartListBasedOnCustomer(Long customerId) throws ValidationException, NotFoundException;

	/**
	 * @param customerId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Long getCartItemCount() throws ValidationException, NotFoundException;

	/**
	 * @param customer
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteCart() throws NotFoundException, ValidationException;

	/**
	 * @param customerId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteCartItemForCustomer(Long customerId) throws NotFoundException, ValidationException;

	/**
	 * @param vendorId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean checkIfExistsCartItemWithDifferentVendor(Long vendorId) throws ValidationException, NotFoundException;

	/**
	 * @param uuid
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<CartItemResponseDTO> moveFromTempCartToCart(String uuid) throws NotFoundException, ValidationException;

	/**
	 * delete all cart item based on product variant id : this method is used at the time of deactivating product variant
	 *
	 * @param productVariantId
	 * @throws NotFoundException
	 */
	void deleteCartItemsForProductVariant(Long productVariantId) throws NotFoundException;

	/**
	 * @param onlineOrderId
	 * @throws NotFoundException
	 */
	void deleteCartItemForOnlineOrderId(String onlineOrderId) throws NotFoundException;

	/**
	 * get vendor payment mode and delivery type from cart item
	 *
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	VendorResponseDTO getVendorFromCartItems() throws ValidationException, NotFoundException;

}
