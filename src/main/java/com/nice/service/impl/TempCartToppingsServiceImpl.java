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

import com.nice.dto.ProductToppingResponseDTO;
import com.nice.dto.TempCartToppingsDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.ProductTopping;
import com.nice.model.TempCartItem;
import com.nice.model.TempCartToppings;
import com.nice.repository.TempCartToppingsRepository;
import com.nice.service.ProductToppingService;
import com.nice.service.TempCartItemService;
import com.nice.service.TempCartToppingsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("tempCartToppingsService")
public class TempCartToppingsServiceImpl implements TempCartToppingsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartToppingsServiceImpl.class);
	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private TempCartToppingsRepository tempCartToppingsRepository;

	@Autowired
	private ProductToppingService productAddonsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductToppingService productToppingService;

	@Override
	public void addTempCartToppings(final TempCartToppingsDto tempCartAddonsDTO, final TempCartItem tempCartItem)
			throws ValidationException, NotFoundException {
		TempCartToppings tempCartAddons = new TempCartToppings();
		BeanUtils.copyProperties(tempCartAddonsDTO, tempCartAddons);
		ProductTopping productToppings = productAddonsService.getProductToppingDetails(tempCartAddonsDTO.getProductToppingsId());
		/**
		 * Check if addons belongs to the product variant
		 */
		if (!tempCartItem.getProductVariant().getId().equals(productToppings.getProductVariant().getId())) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.associated.to.variant", null));
		}

		/**
		 * check for existing addons
		 */
		if (checkIfExistsTempCartToppingsForCartItemAndAddons(tempCartItem, productToppings)) {
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				throw new ValidationException(
						messageByLocaleService.getMessage("toppings.exists.temp.cart", new Object[] { productToppings.getTopping().getNameEnglish() }));
			} else {
				throw new ValidationException(
						messageByLocaleService.getMessage("toppings.exists.temp.cart", new Object[] { productToppings.getTopping().getNameArabic() }));
			}
		}
		tempCartAddons.setProductToppings(productToppings);
		tempCartAddons.setTempCartItem(tempCartItem);
		tempCartToppingsRepository.save(tempCartAddons);
	}

	@Override
	public List<ProductToppingResponseDTO> getTempCartToppingsListForCartItem(final Long cartItemId) throws NotFoundException {
		TempCartItem tempCartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartToppings> tempCartAddonsList = tempCartToppingsRepository.findAllByTempCartItem(tempCartItem);
		return convertEntityToDtos(tempCartAddonsList);
	}

	@Override
	public void updateTempCartToppingsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartToppings> tempCartAddonsList = tempCartToppingsRepository.findAllByTempCartItem(cartItem);
		for (TempCartToppings tempCartAddons : tempCartAddonsList) {
			tempCartAddons.setQuantity(quantity);
			tempCartToppingsRepository.save(tempCartAddons);
		}
	}

	@Override
	public void deleteTempCartToppings(final Long cartItemId) throws NotFoundException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		tempCartToppingsRepository.deleteAllByTempCartItem(cartItem);
	}

	/**
	 * @param tempCartAddon
	 * @return
	 */
	private ProductToppingResponseDTO convertEntityToDto(final TempCartToppings tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productToppingService.convertFromEntityToDto(tempCartAddon.getProductToppings());

	}

	/**
	 * @param tempCartAddonsList
	 */
	private List<ProductToppingResponseDTO> convertEntityToDtos(final List<TempCartToppings> tempCartAddonsList) {
		List<ProductToppingResponseDTO> tempCartAddonsDtoList = new ArrayList<>();
		for (TempCartToppings tempCartAddons : tempCartAddonsList) {
			tempCartAddonsDtoList.add(convertEntityToDto(tempCartAddons));
		}
		return tempCartAddonsDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartAddons
	 */
	private boolean checkIfExistsTempCartToppingsForCartItemAndAddons(final TempCartItem tempCartItem, final ProductTopping productAddons) {
		return tempCartToppingsRepository.findAllByTempCartItemAndProductToppings(tempCartItem, productAddons).isPresent();

	}
}
