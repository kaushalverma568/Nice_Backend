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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.TempCartItemDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.TempCartItemService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@RequestMapping(path = "/temp/cart/item")
@RestController
public class TempCartItemController {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartItemController.class);
	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private TempCartItemService tempCartItemService;

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
	public ResponseEntity<Object> addCartItem(@RequestBody @Valid final TempCartItemDTO tempCartItemDTO, final BindingResult result)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Cart Item {}", tempCartItemDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Cart Item validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		Long cartItemId = tempCartItemService.addTempCartItem(tempCartItemDTO);
		LOGGER.info("Outside add Cart Item ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cart.item.create.successfully", null)).setData(cartItemId).create();
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
	public ResponseEntity<Object> getCartItemListByParam(@RequestParam(name = "uuid", required = true) final String uuid)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Cart Item List ");
		final List<CartItemResponseDTO> resultCartItems = tempCartItemService.getTempCartItemDetailListByParam(uuid);
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
	public ResponseEntity<Object> deleteCartItem(@PathVariable("cartItemId") final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside delete Cart Item ");
		tempCartItemService.deleteTempCartItem(cartItemId);
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
	@GetMapping("/count/{uuid}")
	public ResponseEntity<Object> getCartItemCountForUuid(@PathVariable("uuid") final String uuid) throws ValidationException {
		LOGGER.info("Inside get Cart Item Count For uuid Method");
		final Long count = tempCartItemService.getTempCartItemCountForUuid(uuid);
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
	public ResponseEntity<Object> updateCartQty(@PathVariable("cartItemId") final Long cartItemId, @RequestParam(name = "qty", required = true) final Long qty)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Cart Item Qty Method");
		tempCartItemService.updateTempCartItemQty(cartItemId, qty);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("cart.item.quantity.updated", null))
				.create();
	}

	/**
	 * delete all cart items for uuid
	 *
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping("/uuid/{uuid}")
	public ResponseEntity<Object> deleteCartItemByUuid(@PathVariable("uuid") final String uuid) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete Cart Item {}", uuid);
		tempCartItemService.deleteTempCartItemForUuid(uuid);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cart.item.deleted.successfully", null)).create();
	}

	@GetMapping("/check/{uuid}/{vendorId}")
	public ResponseEntity<Object> checkIfCartContainsItemsWithDifferentVendor(@PathVariable("uuid") final String uuid, @PathVariable final Long vendorId)
			throws ValidationException {
		LOGGER.info("Inside delete Cart Item {}", uuid);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setData(tempCartItemService.checkIfExistsCartItemWithDifferentVendor(uuid, vendorId))
				.setMessage(messageByLocaleService.getMessage("cart.item.checked.successfully", null)).create();
	}

}
