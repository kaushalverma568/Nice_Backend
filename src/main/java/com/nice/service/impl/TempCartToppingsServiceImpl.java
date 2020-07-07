package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.ProductToppingDto;
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
		ProductTopping productAddons = productAddonsService.getProductToppingDetails(tempCartAddonsDTO.getProductToppingsId());
		tempCartAddons.setProductToppings(productAddons);
		tempCartAddons.setTempCartItem(tempCartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsTempCartToppingsForCartItemAndAddons(tempCartItem, productAddons)) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productAddons.getName() }));
		}
		tempCartToppingsRepository.save(tempCartAddons);
	}

	@Override
	public List<ProductToppingDto> getTempCartToppingsListForCartItem(final Long cartItemId) throws NotFoundException {
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
	 *
	 * @param tempCartAddon
	 * @return
	 */
	private ProductToppingDto convertEntityToDto(final TempCartToppings tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productToppingService.convertFromEntityToDto(tempCartAddon.getProductToppings());

	}

	/**
	 * @param tempCartAddonsList
	 */
	private List<ProductToppingDto> convertEntityToDtos(final List<TempCartToppings> tempCartAddonsList) {
		List<ProductToppingDto> tempCartAddonsDtoList = new ArrayList<>();
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
