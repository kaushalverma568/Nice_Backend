package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CartExtrasDto;
import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMapper;
import com.nice.model.CartExtras;
import com.nice.model.CartItem;
import com.nice.model.ProductExtras;
import com.nice.repository.CartExtrasRepository;
import com.nice.service.CartExtrasService;
import com.nice.service.CartItemService;
import com.nice.service.ProductExtrasService;

@Transactional(rollbackFor = Throwable.class)
@Service("cartExtrasService")
public class CartExtrasServiceImpl implements CartExtrasService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartExtrasServiceImpl.class);
	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private CartExtrasRepository cartExtrasRepository;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Autowired
	private ProductExtrasMapper productExtrasMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addCartExtras(final CartExtrasDto cartExtrasDTO, final CartItem cartItem) throws ValidationException, NotFoundException {
		CartExtras cartExtras = new CartExtras();
		BeanUtils.copyProperties(cartExtrasDTO, cartExtras);
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(cartExtrasDTO.getProductExtrasId());
		cartExtras.setProductExtras(productExtras);
		cartExtras.setCartItem(cartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartExtrasForCartItemAndExtras(cartItem, productExtras)) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productExtras.getName() }));
		}
		cartExtrasRepository.save(cartExtras);
	}

	@Override
	public List<ProductExtrasDTO> getCartExtrasListForCartItem(final Long cartItemId) throws NotFoundException {
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartExtras> cartExtrasList = cartExtrasRepository.findAllByCartItem(cartItem);
		return convertEntityToDtos(cartExtrasList);
	}

	@Override
	public void updateCartExtrasQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartExtras> cartExtrasList = cartExtrasRepository.findAllByCartItem(cartItem);
		for (CartExtras cartExtras : cartExtrasList) {
			cartExtras.setQuantity(quantity);
			cartExtrasRepository.save(cartExtras);
		}
	}

	@Override
	public void deleteCartExtras(final Long cartItemId) throws NotFoundException {
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartExtrasRepository.deleteAllByCartItem(cartItem);
	}

	/**
	 *
	 * @param cartAddon
	 * @return
	 */
	private ProductExtrasDTO convertEntityToDto(final CartExtras cartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productExtrasMapper.toDto(cartAddon.getProductExtras());
	}

	/**
	 * @param cartExtrasList
	 */
	private List<ProductExtrasDTO> convertEntityToDtos(final List<CartExtras> cartExtrasList) {
		List<ProductExtrasDTO> productExtrasDtoList = new ArrayList<>();
		for (CartExtras cartExtras : cartExtrasList) {
			productExtrasDtoList.add(convertEntityToDto(cartExtras));
		}
		return productExtrasDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartExtras
	 */
	private boolean checkIfExistsCartExtrasForCartItemAndExtras(final CartItem cartItem, final ProductExtras productExtras) {
		return cartExtrasRepository.findAllByCartItemAndProductExtras(cartItem, productExtras).isPresent();

	}
}
