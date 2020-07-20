package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CartToppingsDto;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.CartItem;
import com.nice.model.CartToppings;
import com.nice.model.ProductTopping;
import com.nice.repository.CartToppingsRepository;
import com.nice.service.CartItemService;
import com.nice.service.CartToppingsService;
import com.nice.service.ProductToppingService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("cartToppingsService")
public class CartToppingsServiceImpl implements CartToppingsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartToppingsServiceImpl.class);
	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private CartToppingsRepository cartToppingsRepository;

	@Autowired
	private ProductToppingService productToppingsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductToppingService productToppingService;

	@Override
	public void addCartToppings(final CartToppingsDto cartToppingsDTO, final CartItem cartItem) throws ValidationException, NotFoundException {
		LOGGER.info("Saving data to cart topping for cartItemId :{}", cartItem.getId());
		CartToppings cartToppings = new CartToppings();
		BeanUtils.copyProperties(cartToppingsDTO, cartToppings);
		ProductTopping productToppings = productToppingsService.getProductToppingDetails(cartToppingsDTO.getProductToppingsId());
		/**
		 * Check if Toppings belongs to the product variant
		 */
		if (!cartItem.getProductVariant().getId().equals(productToppings.getProductVariant().getId())) {
			LOGGER.error("Topping not associated to product variant");
			throw new ValidationException(messageByLocaleService.getMessage("topping.associated.to.variant", null));
		}
		/**
		 * check for existing Toppings
		 */
		if (checkIfExistsCartToppingsForCartItemAndToppings(cartItem, productToppings)) {
			throw new ValidationException(
					messageByLocaleService.getMessage("toppings.exists.temp.cart", new Object[] { productToppings.getTopping().getName() }));
		}
		cartToppings.setProductToppings(productToppings);
		cartToppings.setCartItem(cartItem);
		LOGGER.error("All validations passed, saving topping data for cartItem :{}", cartItem.getId());
		cartToppingsRepository.save(cartToppings);
	}

	@Override
	public List<ProductToppingResponseDTO> getProductToppingsDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside getProductToppingsDtoListForCartItem cartItemId :{}", cartItemId);
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartToppings> cartToppingsList = cartToppingsRepository.findAllByCartItem(cartItem);
		LOGGER.info("After getProductToppingsDtoListForCartItem cartItemId :{}", cartItemId);
		return convertEntityToDtos(cartToppingsList);
	}

	@Override
	public void updateCartToppingsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateCartToppingsQty cartItemId :{}, and qty :{}", cartItemId, quantity);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartToppings> cartToppingsList = cartToppingsRepository.findAllByCartItem(cartItem);
		for (CartToppings cartToppings : cartToppingsList) {
			cartToppings.setQuantity(quantity);
			cartToppingsRepository.save(cartToppings);
		}
		LOGGER.info("After successfully updateCartToppingsQty cartItemId :{} with qty :{}", cartItemId, quantity);
	}

	@Override
	public void deleteCartToppings(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside deleteCartToppings for cartItemId :{}", cartItemId);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartToppingsRepository.deleteAllByCartItem(cartItem);
		LOGGER.info("After deleteCartToppings for cartItemId :{}", cartItemId);
	}

	@Override
	public void deleteCartToppingsById(final Long cartToppingId) throws NotFoundException {
		LOGGER.info("Inside deleteCartToppings for cartToppingId :{}", cartToppingId);
		cartToppingsRepository.deleteById(cartToppingId);
	}

	/**
	 * @param  cartAddon
	 * @return
	 */
	private ProductToppingResponseDTO convertEntityToDto(final CartToppings cartTopping) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productToppingService.convertFromEntityToDto(cartTopping.getProductToppings());

	}

	/**
	 * @param cartToppingsList
	 */
	private List<ProductToppingResponseDTO> convertEntityToDtos(final List<CartToppings> cartToppingsList) {
		LOGGER.info("Inside convertEntityToDtos");
		List<ProductToppingResponseDTO> cartToppingsDtoList = new ArrayList<>();
		for (CartToppings cartToppings : cartToppingsList) {
			cartToppingsDtoList.add(convertEntityToDto(cartToppings));
		}
		LOGGER.info("After convertEntityToDtos");
		return cartToppingsDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartToppings
	 */
	private boolean checkIfExistsCartToppingsForCartItemAndToppings(final CartItem cartItem, final ProductTopping productToppings) {
		LOGGER.info("Inside checkIfExistsCartToppingsForCartItemAndToppings for cartItem : {} and productToppings :{}", cartItem.getId(), productToppings);
		return cartToppingsRepository.findAllByCartItemAndProductToppings(cartItem, productToppings).isPresent();
	}

	@Override
	public List<CartToppings> getCartToppingsListForCartItem(final Long id) throws NotFoundException {
		LOGGER.info("Inside getCartToppingsListForCartItem using Id for cartItem : {}", id);
		CartItem cartItem = cartItemService.getCartItemDetail(id);
		LOGGER.info("After getCartToppingsListForCartItem using Id for cartItem : {}", id);
		return getCartToppingsListForCartItem(cartItem);
	}

	@Override
	public List<CartToppings> getCartToppingsListForCartItem(final CartItem cartItem) {
		LOGGER.info("Inside getCartToppingsListForCartItem using Object for cartItem : {}", cartItem.getId());
		return cartToppingsRepository.findAllByCartItem(cartItem);
	}

	@Override
	public List<CartToppings> getCartToppingsListBasedOnProductTopping(final ProductTopping productTopping) {
		return cartToppingsRepository.findAllByProductToppings(productTopping);
	}
}
