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
import com.nice.dto.ProductToppingDto;
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

@Transactional(rollbackFor = Throwable.class)
@Service("cartToppingsService")
public class CartToppingsServiceImpl implements CartToppingsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartToppingsServiceImpl.class);
	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private CartToppingsRepository cartToppingsRepository;

	@Autowired
	private ProductToppingService productAddonsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductToppingService productToppingService;

	@Override
	public void addCartToppings(final CartToppingsDto cartAddonsDTO, final CartItem tempCartItem) throws ValidationException, NotFoundException {
		CartToppings cartToppings = new CartToppings();
		BeanUtils.copyProperties(cartAddonsDTO, cartToppings);
		ProductTopping productAddons = productAddonsService.getProductToppingDetails(cartAddonsDTO.getProductToppingsId());
		cartToppings.setProductToppings(productAddons);
		cartToppings.setCartItem(tempCartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartToppingsForCartItemAndAddons(tempCartItem, productAddons)) {
			throw new ValidationException(messageByLocaleService.getMessage("toppings.exists.temp.cart", new Object[] { productAddons.getName() }));
		}
		cartToppingsRepository.save(cartToppings);
	}

	@Override
	public List<ProductToppingDto> getProductToppingsDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		CartItem tempCartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartToppings> tempCartAddonsList = cartToppingsRepository.findAllByCartItem(tempCartItem);
		return convertEntityToDtos(tempCartAddonsList);
	}

	@Override
	public void updateCartToppingsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartToppings> tempCartAddonsList = cartToppingsRepository.findAllByCartItem(cartItem);
		for (CartToppings tempCartAddons : tempCartAddonsList) {
			tempCartAddons.setQuantity(quantity);
			cartToppingsRepository.save(tempCartAddons);
		}
	}

	@Override
	public void deleteCartToppings(final Long cartItemId) throws NotFoundException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartToppingsRepository.deleteAllByCartItem(cartItem);
	}

	/**
	 *
	 * @param tempCartAddon
	 * @return
	 */
	private ProductToppingDto convertEntityToDto(final CartToppings tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productToppingService.convertFromEntityToDto(tempCartAddon.getProductToppings());

	}

	/**
	 * @param cartAddonsList
	 */
	private List<ProductToppingDto> convertEntityToDtos(final List<CartToppings> cartAddonsList) {
		List<ProductToppingDto> cartAddonsDtoList = new ArrayList<>();
		for (CartToppings tempCartAddons : cartAddonsList) {
			cartAddonsDtoList.add(convertEntityToDto(tempCartAddons));
		}
		return cartAddonsDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartAddons
	 */
	private boolean checkIfExistsCartToppingsForCartItemAndAddons(final CartItem tempCartItem, final ProductTopping productAddons) {
		return cartToppingsRepository.findAllByCartItemAndProductToppings(tempCartItem, productAddons).isPresent();

	}

	@Override
	public List<CartToppings> getCartToppingsListForCartItem(final Long id) throws NotFoundException {
		CartItem cartItem = cartItemService.getCartItemDetail(id);
		return getCartToppingsListForCartItem(cartItem);
	}

	@Override
	public List<CartToppings> getCartToppingsListForCartItem(final CartItem cartItem) {
		return cartToppingsRepository.findAllByCartItem(cartItem);
	}
}
