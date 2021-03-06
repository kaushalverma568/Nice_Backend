package com.nice.controller;

import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.CartItemDTO;
import com.nice.dto.CartItemResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.CartItemService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@RequestMapping(path = "/cart/item")
@RestController
public class CartItemController {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartItemController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private CartItemService cartItemService;

	/**
	 * add product to cart
	 *
	 * @param accessToken
	 * @param userId
	 * @param cartItemDTO
	 * @param result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping()
	public ResponseEntity<Object> addCartItem(@RequestHeader("Authorization") final String accessToken, @RequestBody @Valid final CartItemDTO cartItemDTO,
			final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Cart Item {}", cartItemDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Cart Item validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		List<CartItemResponseDTO> cartItemsList = cartItemService.addCartItem(cartItemDTO);
		LOGGER.info("Outside add Cart Item ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cart.item.create.successfully", null)).setData(cartItemsList).create();
	}

	/**
	 * @param accessToken
	 * @param uuid
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/move/{uuid}")
	public ResponseEntity<Object> moveFromTempCartToCart(@RequestHeader("Authorization") final String accessToken, @PathVariable final String uuid)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside move Cart Item with uuid {}", uuid);
		List<CartItemResponseDTO> cartItemResponseDtoList = cartItemService.moveFromTempCartToCart(uuid);
		LOGGER.info("Outside move Cart Item with uuid {}", uuid);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(cartItemResponseDtoList)
				.setMessage(messageByLocaleService.getMessage("cart.item.moved.successfully", null)).create();
	}

	/**
	 * Get cart item list based on uuid *
	 *
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/list")
	public ResponseEntity<Object> getCartItemListByParam(@RequestHeader("Authorization") final String accessToken)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Cart Item List ");
		final List<CartItemResponseDTO> resultCartItems = cartItemService.getCartItemDetailList();
		LOGGER.info("Outside get Cart Item List ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cart.item.list.successfully", null))
				.setData(resultCartItems).create();

	}

	/**
	 * Remove cart item by cartItemId
	 *
	 * @param accessToken
	 * @param cartItemId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping("/{cartItemId}")
	public ResponseEntity<Object> deleteCartItem(@RequestHeader("Authorization") final String accessToken, @PathVariable("cartItemId") final Long cartItemId)
			throws NotFoundException {
		LOGGER.info("Inside delete Cart Item for Id :{}", cartItemId);
		cartItemService.deleteCartItem(cartItemId);
		LOGGER.info("Outside delete Cart Item for Id :{}", cartItemId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cart.item.deleted.successfully", null)).create();
	}

	/**
	 * Get cart item count for uuid
	 *
	 * @param accessToken
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/count")
	public ResponseEntity<Object> getCartItemCountForCustomer(@RequestHeader("Authorization") final String token)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside get Cart Item Count For Customer Method");
		final Long count = cartItemService.getCartItemCount();
		LOGGER.info("Inside get Cart Item Count For Customer Method : {}", count);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cart.count.successfully", null))
				.setData(count).create();
	}

	/**
	 * update cart item qty by id
	 *
	 * @param cartItemId
	 * @param qty
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PutMapping("/update/qty/{cartItemId}")
	public ResponseEntity<Object> updateCartQty(@RequestHeader("Authorization") final String accessToken, @PathVariable("cartItemId") final Long cartItemId,
			@RequestParam(name = "qty", required = true) final Long qty) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Cart Item Qty Method for cartItemId : {}, qty : {}", cartItemId, qty);
		List<CartItemResponseDTO> cartItemResponseList = cartItemService.updateCartItemQty(cartItemId, qty);
		LOGGER.info("After successfully update Cart Item Qty Method for cartItemId : {} with qty : {}", cartItemId, qty);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cart.item.quantity.updated", null))
				.setData(cartItemResponseList).create();
	}

	/**
	 * delete all cart items for customer
	 *
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping("/all")
	public ResponseEntity<Object> deleteCartItemForCustomer(@RequestHeader("Authorization") final String accessToken)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete all Cart Item");
		cartItemService.deleteCart();
		LOGGER.info("Inside delete all Cart Item");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cart.item.deleted.successfully", null)).create();
	}

	/**
	 * @param accessToken
	 * @param vendorId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/check/{vendorId}")
	public ResponseEntity<Object> checkIfCartContainsItemsWithDifferentVendor(@RequestHeader("Authorization") final String accessToken,
			@PathVariable final Long vendorId) throws ValidationException, NotFoundException {
		LOGGER.info("Inside check Cart Item with vendor - {}", vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(cartItemService.checkIfExistsCartItemWithDifferentVendor(vendorId))
				.setMessage(messageByLocaleService.getMessage("cart.item.checked.successfully", null)).create();
	}

	/**
	 * get vendor from cart item
	 *
	 * @param accessToken
	 * @param vendorId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@GetMapping("/vendor/detail")
	public ResponseEntity<Object> getVendorFromCartItems(@RequestHeader("Authorization") final String accessToken)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside get vendor from cart items ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setData(cartItemService.getVendorFromCartItems())
				.setMessage(messageByLocaleService.getMessage("cart.item.checked.successfully", null)).create();
	}
}
