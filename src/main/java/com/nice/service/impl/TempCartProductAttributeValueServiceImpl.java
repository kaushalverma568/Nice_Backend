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

import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.TempCartProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAttributeValueMapper;
import com.nice.model.ProductAttributeValue;
import com.nice.model.TempCartItem;
import com.nice.model.TempCartProductAttributeValue;
import com.nice.repository.TempCartProductAttributeValueRepository;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.TempCartItemService;
import com.nice.service.TempCartProductAttributeValueService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("tempCartAttributeValueService")
public class TempCartProductAttributeValueServiceImpl implements TempCartProductAttributeValueService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartProductAttributeValueServiceImpl.class);
	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private TempCartProductAttributeValueRepository tempCartAttributeValueRepository;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private ProductAttributeValueMapper productAttributeValueMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addTempCartProductAttributeValue(final TempCartProductAttributeValueDTO tempCartAttributeValueDTO, final TempCartItem tempCartItem)
			throws ValidationException, NotFoundException {
		TempCartProductAttributeValue tempCartAttributeValue = new TempCartProductAttributeValue();
		BeanUtils.copyProperties(tempCartAttributeValueDTO, tempCartAttributeValue);
		ProductAttributeValue productAttributeValue = productAttributeValueService
				.getProductAttributeValueDetail(tempCartAttributeValueDTO.getProductAttributeValueId());
		/**
		 * Check if addons belongs to the product variant
		 */
		if (!tempCartItem.getProductVariant().getId().equals(productAttributeValue.getProductVariant().getId())) {
			throw new ValidationException(messageByLocaleService.getMessage("attribute.values.associated.to.variant", null));
		}

		tempCartAttributeValue.setProductAttributeValue(productAttributeValue);
		tempCartAttributeValue.setTempCartItem(tempCartItem);

		/**
		 * check for existing product attribute values
		 */
		if (checkIfExistsTempCartProductAttributeValueForCartItemAndAttributeValue(tempCartItem, productAttributeValue)) {
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.exists.temp.cart",
						new Object[] { productAttributeValue.getProductAttribute().getNameEnglish() }));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.exists.temp.cart",
						new Object[] { productAttributeValue.getProductAttribute().getNameArabic() }));
			}
		}
		tempCartAttributeValueRepository.save(tempCartAttributeValue);
	}

	@Override
	public List<ProductAttributeValueDTO> getTempCartProductAttributeValueListForCartItem(final Long cartItemId) throws NotFoundException {
		TempCartItem tempCartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartProductAttributeValue> tempCartAttributeValueList = tempCartAttributeValueRepository.findAllByTempCartItem(tempCartItem);
		return convertEntityToDtos(tempCartAttributeValueList);
	}

	@Override
	public void updateTempCartProductAttributeValueQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartProductAttributeValue> tempCartAttributeValueList = tempCartAttributeValueRepository.findAllByTempCartItem(cartItem);
		for (TempCartProductAttributeValue tempCartAttributeValue : tempCartAttributeValueList) {
			tempCartAttributeValue.setQuantity(quantity);
			tempCartAttributeValueRepository.save(tempCartAttributeValue);
		}
	}

	@Override
	public void deleteTempCartProductAttributeValue(final Long cartItemId) throws NotFoundException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		tempCartAttributeValueRepository.deleteAllByTempCartItem(cartItem);
	}

	/**
	 * @param tempCartAddon
	 * @return
	 */
	private ProductAttributeValueDTO convertEntityToDto(final TempCartProductAttributeValue tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productAttributeValueMapper.toDto(tempCartAddon.getProductAttributeValue());
	}

	/**
	 * @param tempCartAttributeValueList
	 */
	private List<ProductAttributeValueDTO> convertEntityToDtos(final List<TempCartProductAttributeValue> tempCartAttributeValueList) {
		List<ProductAttributeValueDTO> productAttributeValueDtoList = new ArrayList<>();
		for (TempCartProductAttributeValue tempCartAttributeValue : tempCartAttributeValueList) {
			productAttributeValueDtoList.add(convertEntityToDto(tempCartAttributeValue));
		}
		return productAttributeValueDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartAttributeValue
	 */
	private boolean checkIfExistsTempCartProductAttributeValueForCartItemAndAttributeValue(final TempCartItem tempCartItem,
			final ProductAttributeValue productAttributeValue) {
		return tempCartAttributeValueRepository.getCountByTempCartItemAndProductAttribute(tempCartItem.getId(),
				productAttributeValue.getProductAttribute().getId()) > 0L;

	}
}
