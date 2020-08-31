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

import com.nice.dto.CartExtrasDto;
import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMapper;
import com.nice.model.CartExtras;
import com.nice.model.CartItem;
import com.nice.model.ProductExtras;
import com.nice.model.ProductVariant;
import com.nice.repository.CartExtrasRepository;
import com.nice.service.CartExtrasService;
import com.nice.service.CartItemService;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductVariantService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
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

	@Autowired
	private ProductVariantService productVariantService;

	@Override
	public void addCartExtras(final CartExtrasDto cartExtrasDTO, final CartItem cartItem) throws ValidationException, NotFoundException {
		LOGGER.info("Saving data to cart extra for cartItemId :{}", cartItem.getId());
		CartExtras cartExtras = new CartExtras();
		BeanUtils.copyProperties(cartExtrasDTO, cartExtras);
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(cartExtrasDTO.getProductExtrasId());
		ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItem.getProductVariant().getId());
		/**
		 * Check if addons belongs to the product variant
		 */
		if (!productVariant.getProduct().getId().equals(productExtras.getProduct().getId())) {
			LOGGER.error("extra not associated to product variant");
			throw new ValidationException(messageByLocaleService.getMessage("extras.associated.to.product", null));
		}

		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartExtrasForCartItemAndExtras(cartItem, productExtras)) {

			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				throw new ValidationException(
						messageByLocaleService.getMessage("extras.exists.temp.cart", new Object[] { productExtras.getProductExtrasMaster().getNameEnglish() }));
			} else {
				throw new ValidationException(
						messageByLocaleService.getMessage("extras.exists.temp.cart", new Object[] { productExtras.getProductExtrasMaster().getNameArabic() }));
			}
		}
		cartExtras.setProductExtras(productExtras);
		cartExtras.setCartItem(cartItem);
		LOGGER.error("All validations passed, saving extra data for cartItem :{}", cartItem.getId());
		cartExtrasRepository.save(cartExtras);
	}

	@Override
	public List<ProductExtrasDTO> getCartExtrasDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside getProductextrasDtoListForCartItem cartItemId :{}", cartItemId);
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartExtras> cartExtrasList = cartExtrasRepository.findAllByCartItem(cartItem);
		LOGGER.info("After getProductextrasDtoListForCartItem cartItemId :{}", cartItemId);
		return convertEntityToDtos(cartExtrasList);
	}

	@Override
	public void updateCartExtrasQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateCartextrasQty cartItemId :{}, and qty :{}", cartItemId, quantity);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartExtras> cartExtrasList = cartExtrasRepository.findAllByCartItem(cartItem);
		for (CartExtras cartExtras : cartExtrasList) {
			cartExtras.setQuantity(quantity);
			cartExtrasRepository.save(cartExtras);
		}
		LOGGER.info("After successfully updateCartextrasQty cartItemId :{} with qty :{}", cartItemId, quantity);
	}

	@Override
	public void deleteCartExtras(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside deleteCartextras for cartItemId :{}", cartItemId);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartExtrasRepository.deleteAllByCartItem(cartItem);
		LOGGER.info("After deleteCartextras for cartItemId :{}", cartItemId);
	}

	/**
	 * @param cartExtras
	 * @return
	 */
	private ProductExtrasDTO convertEntityToDto(final CartExtras cartExtras) {
		LOGGER.info("Inside convert Entity To Dto method");
		ProductExtrasDTO productExtrasDto = productExtrasMapper.toDto(cartExtras.getProductExtras());
		productExtrasDto.setQuantity(cartExtras.getQuantity());
		return productExtrasDto;
	}

	/**
	 * @param cartExtrasList
	 */
	private List<ProductExtrasDTO> convertEntityToDtos(final List<CartExtras> cartExtrasList) {
		LOGGER.info("Inside convertEntityToDtos");
		List<ProductExtrasDTO> productExtrasDtoList = new ArrayList<>();
		for (CartExtras cartExtras : cartExtrasList) {
			productExtrasDtoList.add(convertEntityToDto(cartExtras));
		}
		LOGGER.info("After convertEntityToDtos");
		return productExtrasDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartExtras
	 */
	private boolean checkIfExistsCartExtrasForCartItemAndExtras(final CartItem cartItem, final ProductExtras productExtras) {
		LOGGER.info("Inside checkIfExistsCartextrasForCartItemAndextras for cartItem : {} and productextras :{}", cartItem.getId(), productExtras);
		return cartExtrasRepository.findAllByCartItemAndProductExtras(cartItem, productExtras).isPresent();

	}

	@Override
	public List<CartExtras> getCartExtrasListForCartItem(final Long id) throws NotFoundException {
		LOGGER.info("Inside getCartextrasListForCartItem using Id for cartItem : {}", id);
		CartItem cartItem = cartItemService.getCartItemDetail(id);
		LOGGER.info("After getCartextrasListForCartItem using Id for cartItem : {}", id);
		return getCartExtrasListForCartItem(cartItem);
	}

	@Override
	public void deleteCartExtrasByExtrasId(final Long productExtrasId) throws NotFoundException {
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(productExtrasId);
		cartExtrasRepository.deleteByProductExtras(productExtras);
	}

	@Override
	public List<CartExtras> getCartExtrasListForCartItem(final CartItem cartItem) {
		LOGGER.info("Inside getCartextrasListForCartItem using Object for cartItem : {}", cartItem.getId());
		return cartExtrasRepository.findAllByCartItem(cartItem);
	}

	@Override
	public List<CartExtras> getCartExtrasListFromProductExtrasId(final Long productExtrasId) throws NotFoundException {
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(productExtrasId);
		return cartExtrasRepository.findAllByProductExtras(productExtras);
	}

}
