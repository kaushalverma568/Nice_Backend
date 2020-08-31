package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
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

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
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
		LOGGER.info("Saving data to cart addon for cartItemId :{}", cartItem.getId());
		CartAddons cartAddons = new CartAddons();
		BeanUtils.copyProperties(cartAddonsDTO, cartAddons);
		ProductAddons productAddons = productAddonsService.getProductAddonsDetail(cartAddonsDTO.getProductAddonsId());

		/**
		 * Check if addons belongs to the product variant
		 */
		if (!cartItem.getProductVariant().getId().equals(productAddons.getProductVariant().getId())) {
			LOGGER.error("addon not associated to product variant");
			throw new ValidationException(messageByLocaleService.getMessage("addons.associated.to.variant", null));
		}
		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartAddonsForCartItemAndAddons(cartItem, productAddons)) {
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				throw new ValidationException(
						messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productAddons.getAddons().getNameEnglish() }));
			} else {
				throw new ValidationException(
						messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productAddons.getAddons().getNameArabic() }));
			}
		}
		cartAddons.setProductAddons(productAddons);
		cartAddons.setCartItem(cartItem);
		LOGGER.error("All validations passed, saving addon data for cartItem :{}", cartItem.getId());
		cartAddonsRepository.save(cartAddons);
	}

	@Override
	public List<ProductAddonsDTO> getCartAddonsDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside getProductAddonsDtoListForCartItem cartItemId :{}", cartItemId);
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartAddons> cartAddonsList = cartAddonsRepository.findAllByCartItem(cartItem);
		LOGGER.info("After getProductAddonsDtoListForCartItem cartItemId :{}", cartItemId);
		return convertEntityToDtos(cartAddonsList);
	}

	@Override
	public List<CartAddons> getCartAddonsListForCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside getCartAddonsListForCartItem using Id for cartItem : {}", cartItemId);
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		LOGGER.info("After getCartAddonsListForCartItem using Id for cartItem : {}", cartItemId);
		return getCartAddonsListForCartItem(cartItem);
	}

	@Override
	public List<CartAddons> getCartAddonsListForCartItem(final CartItem cartItem) {
		LOGGER.info("Inside getCartAddonsListForCartItem using Object for cartItem : {}", cartItem.getId());
		return cartAddonsRepository.findAllByCartItem(cartItem);
	}

	@Override
	public void updateCartAddonsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateCartAddonsQty cartItemId :{}, and qty :{}", cartItemId, quantity);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartAddons> cartAddonsList = cartAddonsRepository.findAllByCartItem(cartItem);
		for (CartAddons cartAddons : cartAddonsList) {
			cartAddons.setQuantity(quantity);
			cartAddonsRepository.save(cartAddons);
		}
		LOGGER.info("After successfully updateCartAddonsQty cartItemId :{} with qty :{}", cartItemId, quantity);
	}

	@Override
	public void deleteCartAddons(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside deleteCartAddons for cartItemId :{}", cartItemId);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartAddonsRepository.deleteAllByCartItem(cartItem);
		LOGGER.info("After deleteCartAddons for cartItemId :{}", cartItemId);
	}

	/**
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
		LOGGER.info("Inside convertEntityToDtos");
		List<ProductAddonsDTO> productAddonsDtoList = new ArrayList<>();
		for (CartAddons cartAddons : cartAddonsList) {
			productAddonsDtoList.add(convertEntityToDto(cartAddons));
		}
		LOGGER.info("After convertEntityToDtos");
		return productAddonsDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartAddons
	 */
	private boolean checkIfExistsCartAddonsForCartItemAndAddons(final CartItem cartItem, final ProductAddons productAddons) {
		LOGGER.info("Inside checkIfExistsCartAddonsForCartItemAndAddons for cartItem : {} and productAddons :{}", cartItem.getId(), productAddons);
		return cartAddonsRepository.findAllByCartItemAndProductAddons(cartItem, productAddons).isPresent();
	}

	@Override
	public List<CartAddons> getCartAddonsByProductAddOns(final Long productAddonsId) throws NotFoundException {
		return cartAddonsRepository.findAllByProductAddons(productAddonsService.getProductAddonsDetail(productAddonsId));
	}

	@Override
	public void deleteByCartAddonsId(final Long id) {
		cartAddonsRepository.deleteById(id);
	}
}
