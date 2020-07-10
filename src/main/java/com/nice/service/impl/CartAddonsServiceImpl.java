package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CartAddonsDTO;
import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAddonsMapper;
import com.nice.model.CartAddons;
import com.nice.model.CartItem;
import com.nice.model.ProductAddons;
import com.nice.repository.CartAddonsRepository;
import com.nice.service.CartAddonsService;
import com.nice.service.CartItemService;
import com.nice.service.ProductAddonsService;

@Transactional(rollbackFor = Throwable.class)
@Service("cartAddonsService")
public class CartAddonsServiceImpl implements CartAddonsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartAddonsServiceImpl.class);
	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private CartAddonsRepository cartAddonsRepository;

	@Autowired
	private ProductAddonsService productAddonsService;

	@Autowired
	private ProductAddonsMapper productAddonsMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addCartAddons(final CartAddonsDTO cartAddonsDTO, final CartItem cartItem) throws ValidationException, NotFoundException {
		CartAddons cartAddons = new CartAddons();
		BeanUtils.copyProperties(cartAddonsDTO, cartAddons);
		ProductAddons productAddons = productAddonsService.getProductAddonsDetail(cartAddonsDTO.getProductAddonsId());
		cartAddons.setProductAddons(productAddons);
		cartAddons.setCartItem(cartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartAddonsForCartItemAndAddons(cartItem, productAddons)) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productAddons.getName() }));
		}
		cartAddonsRepository.save(cartAddons);
	}

	@Override
	public List<ProductAddonsDTO> getCartAddonsDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartAddons> cartAddonsList = cartAddonsRepository.findAllByCartItem(cartItem);
		return convertEntityToDtos(cartAddonsList);
	}

	@Override
	public List<CartAddons> getCartAddonsListForCartItem(final Long cartItemId) throws NotFoundException {
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		return getCartAddonsListForCartItem(cartItem);
	}

	@Override
	public List<CartAddons> getCartAddonsListForCartItem(final CartItem cartItem) {
		return cartAddonsRepository.findAllByCartItem(cartItem);
	}

	@Override
	public void updateCartAddonsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartAddons> cartAddonsList = cartAddonsRepository.findAllByCartItem(cartItem);
		for (CartAddons cartAddons : cartAddonsList) {
			cartAddons.setQuantity(quantity);
			cartAddonsRepository.save(cartAddons);
		}
	}

	@Override
	public void deleteCartAddons(final Long cartItemId) throws NotFoundException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartAddonsRepository.deleteAllByCartItem(cartItem);
	}

	/**
	 *
	 * @param cartAddon
	 * @return
	 */
	private ProductAddonsDTO convertEntityToDto(final CartAddons cartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productAddonsMapper.toDto(cartAddon.getProductAddons());
	}

	/**
	 * @param cartAddonsList
	 */
	private List<ProductAddonsDTO> convertEntityToDtos(final List<CartAddons> cartAddonsList) {
		List<ProductAddonsDTO> productAddonsDtoList = new ArrayList<>();
		for (CartAddons cartAddons : cartAddonsList) {
			productAddonsDtoList.add(convertEntityToDto(cartAddons));
		}
		return productAddonsDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartAddons
	 */
	private boolean checkIfExistsCartAddonsForCartItemAndAddons(final CartItem cartItem, final ProductAddons productAddons) {
		return cartAddonsRepository.findAllByCartItemAndProductAddons(cartItem, productAddons).isPresent();

	}
}
