package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CartProductAttributeValueDTO;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAttributeValueMapper;
import com.nice.model.CartItem;
import com.nice.model.CartProductAttributeValue;
import com.nice.model.ProductAttributeValue;
import com.nice.repository.CartProductAttributeValueRepository;
import com.nice.service.CartItemService;
import com.nice.service.CartProductAttributeValueService;
import com.nice.service.ProductAttributeValueService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("cartAttributeValueService")
public class CartProductAttributeValueServiceImpl implements CartProductAttributeValueService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartProductAttributeValueServiceImpl.class);
	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private CartProductAttributeValueRepository cartAttributeValueRepository;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private ProductAttributeValueMapper productAttributeValueMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addCartProductAttributeValue(final CartProductAttributeValueDTO cartAttributeValueDTO, final CartItem cartItem)
			throws ValidationException, NotFoundException {
		LOGGER.info("Saving data to cart ProductAttributeValue for cartItemId :{}", cartItem.getId());
		CartProductAttributeValue cartAttributeValue = new CartProductAttributeValue();
		BeanUtils.copyProperties(cartAttributeValueDTO, cartAttributeValue);
		ProductAttributeValue productAttributeValue = productAttributeValueService
				.getProductAttributeValueDetail(cartAttributeValueDTO.getProductAttributeValueId());
		/**
		 * Check if addons belongs to the product variant
		 */
		if (!cartItem.getProductVariant().getId().equals(productAttributeValue.getProductVariant().getId())) {
			LOGGER.error("ProductAttributeValue not associated to product variant");
			throw new ValidationException(messageByLocaleService.getMessage("attribute.values.associated.to.variant", null));
		}
		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartProductAttributeValueForCartItemAndAttributeValue(cartItem, productAttributeValue)) {
			throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.exists.temp.cart",
					new Object[] { productAttributeValue.getProductAttribute().getName() }));
		}
		cartAttributeValue.setProductAttributeValue(productAttributeValue);
		cartAttributeValue.setCartItem(cartItem);
		LOGGER.error("All validations passed, saving ProductAttributeValue data for cartItem :{}", cartItem.getId());
		cartAttributeValueRepository.save(cartAttributeValue);
	}

	@Override
	public List<ProductAttributeValueDTO> getProductAttributeValueDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside getProductProductAttributeValuesDtoListForCartItem cartItemId :{}", cartItemId);
		CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartProductAttributeValue> cartAttributeValueList = cartAttributeValueRepository.findAllByCartItem(cartItem);
		LOGGER.info("After getProductProductAttributeValuesDtoListForCartItem cartItemId :{}", cartItemId);
		return convertEntityToDtos(cartAttributeValueList);
	}

	@Override
	public void updateCartProductAttributeValueQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateCartProductAttributeValuesQty cartItemId :{}, and qty :{}", cartItemId, quantity);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		List<CartProductAttributeValue> cartAttributeValueList = cartAttributeValueRepository.findAllByCartItem(cartItem);
		for (CartProductAttributeValue cartAttributeValue : cartAttributeValueList) {
			cartAttributeValue.setQuantity(quantity);
			cartAttributeValueRepository.save(cartAttributeValue);
		}
		LOGGER.info("After successfully updateCartProductAttributeValuesQty cartItemId :{} with qty :{}", cartItemId, quantity);
	}

	@Override
	public void deleteCartProductAttributeValue(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside deleteCartProductAttributeValues for cartItemId :{}", cartItemId);
		final CartItem cartItem = cartItemService.getCartItemDetail(cartItemId);
		cartAttributeValueRepository.deleteAllByCartItem(cartItem);
		LOGGER.info("After deleteCartProductAttributeValues for cartItemId :{}", cartItemId);
	}

	/**
	 * @param  cartAddon
	 * @return
	 */
	private ProductAttributeValueDTO convertEntityToDto(final CartProductAttributeValue cartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productAttributeValueMapper.toDto(cartAddon.getProductAttributeValue());
	}

	/**
	 * @param cartAttributeValueList
	 */
	private List<ProductAttributeValueDTO> convertEntityToDtos(final List<CartProductAttributeValue> cartAttributeValueList) {
		LOGGER.info("Inside convertEntityToDtos");
		List<ProductAttributeValueDTO> productAttributeValueDtoList = new ArrayList<>();
		for (CartProductAttributeValue cartAttributeValue : cartAttributeValueList) {
			productAttributeValueDtoList.add(convertEntityToDto(cartAttributeValue));
		}
		LOGGER.info("After convertEntityToDtos");
		return productAttributeValueDtoList;
	}

	/**
	 * @param cartItem
	 * @param cartAttributeValue
	 */
	private boolean checkIfExistsCartProductAttributeValueForCartItemAndAttributeValue(final CartItem cartItem,
			final ProductAttributeValue productAttributeValue) {
		LOGGER.info(
				"Inside checkIfExistsCartProductAttributeValuesForCartItemAndProductAttributeValues for cartItem : {} and productProductAttributeValues :{}",
				cartItem.getId(), productAttributeValue);
		return cartAttributeValueRepository.getCountByCartItemAndProductAttribute(cartItem.getId(), productAttributeValue.getProductAttribute().getId()) > 0L;

	}

	@Override
	public List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(final Long id) throws NotFoundException {
		LOGGER.info("Inside getCartProductAttributeValuesListForCartItem using Id for cartItem : {}", id);
		CartItem cartItem = cartItemService.getCartItemDetail(id);
		LOGGER.info("After getCartProductAttributeValuesListForCartItem using Id for cartItem : {}", id);
		return cartAttributeValueRepository.findAllByCartItem(cartItem);
	}

	@Override
	public List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(final CartItem cartItem) {
		LOGGER.info("Inside getCartProductAttributeValuesListForCartItem using Object for cartItem : {}", cartItem.getId());
		return cartAttributeValueRepository.findAllByCartItem(cartItem);
	}

}
