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

@Transactional(rollbackFor = Throwable.class)
@Service("cartAttributeValueService")
public class CartProductAttributeValueServiceImpl implements CartProductAttributeValueService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartProductAttributeValueServiceImpl.class);
	@Autowired
	private CartItemService tempCartItemService;

	@Autowired
	private CartProductAttributeValueRepository cartAttributeValueRepository;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private ProductAttributeValueMapper productAttributeValueMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addCartProductAttributeValue(final CartProductAttributeValueDTO cartAttributeValueDTO, final CartItem tempCartItem)
			throws ValidationException, NotFoundException {
		CartProductAttributeValue cartAttributeValue = new CartProductAttributeValue();
		BeanUtils.copyProperties(cartAttributeValueDTO, cartAttributeValue);
		ProductAttributeValue productAttributeValue = productAttributeValueService
				.getProductAttributeValueDetail(cartAttributeValueDTO.getProductAttributeValueId());
		cartAttributeValue.setProductAttributeValue(productAttributeValue);
		cartAttributeValue.setCartItem(tempCartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsCartProductAttributeValueForCartItemAndAttributeValue(tempCartItem, productAttributeValue)) {
			throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.exists.temp.cart",
					new Object[] { productAttributeValue.getAttributeValue(), productAttributeValue.getProductAttribute().getName() }));
		}
		cartAttributeValueRepository.save(cartAttributeValue);
	}

	@Override
	public List<ProductAttributeValueDTO> getProductAttributeValueDtoListForCartItem(final Long cartItemId) throws NotFoundException {
		CartItem tempCartItem = tempCartItemService.getCartItemDetail(cartItemId);
		List<CartProductAttributeValue> tempCartAttributeValueList = cartAttributeValueRepository.findAllByCartItem(tempCartItem);
		return convertEntityToDtos(tempCartAttributeValueList);
	}

	@Override
	public void updateCartProductAttributeValueQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final CartItem cartItem = tempCartItemService.getCartItemDetail(cartItemId);
		List<CartProductAttributeValue> tempCartAttributeValueList = cartAttributeValueRepository.findAllByCartItem(cartItem);
		for (CartProductAttributeValue tempCartAttributeValue : tempCartAttributeValueList) {
			tempCartAttributeValue.setQuantity(quantity);
			cartAttributeValueRepository.save(tempCartAttributeValue);
		}
	}

	@Override
	public void deleteCartProductAttributeValue(final Long cartItemId) throws NotFoundException {
		final CartItem cartItem = tempCartItemService.getCartItemDetail(cartItemId);
		cartAttributeValueRepository.deleteAllByCartItem(cartItem);
	}

	/**
	 *
	 * @param tempCartAddon
	 * @return
	 */
	private ProductAttributeValueDTO convertEntityToDto(final CartProductAttributeValue tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productAttributeValueMapper.toDto(tempCartAddon.getProductAttributeValue());
	}

	/**
	 * @param tempCartAttributeValueList
	 */
	private List<ProductAttributeValueDTO> convertEntityToDtos(final List<CartProductAttributeValue> tempCartAttributeValueList) {
		List<ProductAttributeValueDTO> productAttributeValueDtoList = new ArrayList<>();
		for (CartProductAttributeValue tempCartAttributeValue : tempCartAttributeValueList) {
			productAttributeValueDtoList.add(convertEntityToDto(tempCartAttributeValue));
		}
		return productAttributeValueDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartAttributeValue
	 */
	private boolean checkIfExistsCartProductAttributeValueForCartItemAndAttributeValue(final CartItem tempCartItem,
			final ProductAttributeValue productAttributeValue) {
		return cartAttributeValueRepository.findAllByCartItemAndProductAttributeValue(tempCartItem, productAttributeValue).isPresent();

	}

	@Override
	public List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(final Long id) throws NotFoundException {
		CartItem cartItem = tempCartItemService.getCartItemDetail(id);
		return cartAttributeValueRepository.findAllByCartItem(cartItem);
	}

	@Override
	public List<CartProductAttributeValue> getCartProductAttributeValueListForCartItem(final CartItem cartItem) {
		return cartAttributeValueRepository.findAllByCartItem(cartItem);
	}

}
