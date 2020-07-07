package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.ProductAddonsDTO;
import com.nice.dto.TempCartAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAddonsMapper;
import com.nice.model.ProductAddons;
import com.nice.model.TempCartAddons;
import com.nice.model.TempCartItem;
import com.nice.repository.TempCartAddonsRepository;
import com.nice.service.ProductAddonsService;
import com.nice.service.TempCartAddonsService;
import com.nice.service.TempCartItemService;

@Transactional(rollbackFor = Throwable.class)
@Service("tempCartAddonsService")
public class TempCartAddonsServiceImpl implements TempCartAddonsService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartAddonsServiceImpl.class);
	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private TempCartAddonsRepository tempCartAddonsRepository;

	@Autowired
	private ProductAddonsService productAddonsService;

	@Autowired
	private ProductAddonsMapper productAddonsMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addTempCartAddons(final TempCartAddonsDTO tempCartAddonsDTO, final TempCartItem tempCartItem) throws ValidationException, NotFoundException {
		TempCartAddons tempCartAddons = new TempCartAddons();
		BeanUtils.copyProperties(tempCartAddonsDTO, tempCartAddons);
		ProductAddons productAddons = productAddonsService.getProductAddonsDetail(tempCartAddonsDTO.getProductAddonsId());
		tempCartAddons.setProductAddons(productAddons);
		tempCartAddons.setTempCartItem(tempCartItem);

		/**
		 * check for existing addons
		 */
		if (checkIfExistsTempCartAddonsForCartItemAndAddons(tempCartItem, productAddons)) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productAddons.getName() }));
		}
		tempCartAddonsRepository.save(tempCartAddons);
	}

	@Override
	public List<ProductAddonsDTO> getTempCartAddonsListForCartItem(final Long cartItemId) throws NotFoundException {
		TempCartItem tempCartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartAddons> tempCartAddonsList = tempCartAddonsRepository.findAllByTempCartItem(tempCartItem);
		return convertEntityToDtos(tempCartAddonsList);
	}

	@Override
	public void updateTempCartAddonsQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartAddons> tempCartAddonsList = tempCartAddonsRepository.findAllByTempCartItem(cartItem);
		for (TempCartAddons tempCartAddons : tempCartAddonsList) {
			tempCartAddons.setQuantity(quantity);
			tempCartAddonsRepository.save(tempCartAddons);
		}
	}

	@Override
	public void deleteTempCartAddons(final Long cartItemId) throws NotFoundException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		tempCartAddonsRepository.deleteAllByTempCartItem(cartItem);
	}

	/**
	 *
	 * @param tempCartAddon
	 * @return
	 */
	private ProductAddonsDTO convertEntityToDto(final TempCartAddons tempCartAddon) {
		LOGGER.info("Inside convert Entity To Dto method");
		return productAddonsMapper.toDto(tempCartAddon.getProductAddons());
	}

	/**
	 * @param tempCartAddonsList
	 */
	private List<ProductAddonsDTO> convertEntityToDtos(final List<TempCartAddons> tempCartAddonsList) {
		List<ProductAddonsDTO> productAddonsDtoList = new ArrayList<>();
		for (TempCartAddons tempCartAddons : tempCartAddonsList) {
			productAddonsDtoList.add(convertEntityToDto(tempCartAddons));
		}
		return productAddonsDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartAddons
	 */
	private boolean checkIfExistsTempCartAddonsForCartItemAndAddons(final TempCartItem tempCartItem, final ProductAddons productAddons) {
		return tempCartAddonsRepository.findAllByTempCartItemAndProductAddons(tempCartItem, productAddons).isPresent();

	}
}
