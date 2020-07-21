package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.ProductExtrasDTO;
import com.nice.dto.TempCartExtrasDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMapper;
import com.nice.model.ProductExtras;
import com.nice.model.ProductVariant;
import com.nice.model.TempCartExtras;
import com.nice.model.TempCartItem;
import com.nice.repository.TempCartExtrasRepository;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductVariantService;
import com.nice.service.TempCartExtrasService;
import com.nice.service.TempCartItemService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("tempCartExtrasService")
public class TempCartExtrasServiceImpl implements TempCartExtrasService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartExtrasServiceImpl.class);
	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private TempCartExtrasRepository tempCartExtrasRepository;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Autowired
	private ProductExtrasMapper productExtrasMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Override
	public void addTempCartExtras(final TempCartExtrasDto tempCartExtrasDTO, final TempCartItem tempCartItem) throws ValidationException, NotFoundException {
		TempCartExtras tempCartExtras = new TempCartExtras();
		BeanUtils.copyProperties(tempCartExtrasDTO, tempCartExtras);
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(tempCartExtrasDTO.getProductExtrasId());
		ProductVariant productVariant = productVariantService.getProductVariantDetail(tempCartItem.getProductVariant().getId());
		/**
		 * Check if addons belongs to the product variant
		 */
		if (!productVariant.getProduct().getId().equals(productExtras.getProduct().getId())) {
			throw new ValidationException(messageByLocaleService.getMessage("extras.associated.to.product", null));
		}

		/**
		 * check for existing addons
		 */
		if (checkIfExistsTempCartExtrasForCartItemAndExtras(tempCartItem, productExtras)) {
			throw new ValidationException(
					messageByLocaleService.getMessage("addons.exists.temp.cart", new Object[] { productExtras.getProductExtrasMaster().getName() }));
		}
		tempCartExtras.setProductExtras(productExtras);
		tempCartExtras.setTempCartItem(tempCartItem);
		tempCartExtrasRepository.save(tempCartExtras);
	}

	@Override
	public List<ProductExtrasDTO> getTempCartExtrasListForCartItem(final Long cartItemId) throws NotFoundException {
		TempCartItem tempCartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartExtras> tempCartExtrasList = tempCartExtrasRepository.findAllByTempCartItem(tempCartItem);
		return convertEntityToDtos(tempCartExtrasList);
	}

	@Override
	public List<TempCartExtras> getTempCartExtrasListFromProductExtrasId(final Long productExtrasId) throws NotFoundException {
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(productExtrasId);
		return tempCartExtrasRepository.findAllByProductExtras(productExtras);
	}

	@Override
	public void updateTempCartExtrasQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		List<TempCartExtras> tempCartExtrasList = tempCartExtrasRepository.findAllByTempCartItem(cartItem);
		for (TempCartExtras tempCartExtras : tempCartExtrasList) {
			tempCartExtras.setQuantity(quantity);
			tempCartExtrasRepository.save(tempCartExtras);
		}
	}

	@Override
	public void deleteTempCartExtras(final Long cartItemId) throws NotFoundException {
		final TempCartItem cartItem = tempCartItemService.getTempCartItemDetail(cartItemId);
		tempCartExtrasRepository.deleteAllByTempCartItem(cartItem);
	}

	@Override
	public void deleteTempCartExtrasByExtrasId(final Long productExtrasId) throws NotFoundException {
		ProductExtras productExtras = productExtrasService.getProductExtrasDetail(productExtrasId);
		tempCartExtrasRepository.deleteByProductExtras(productExtras);
	}

	/**
	 * @param  tempCartExtras
	 * @return
	 */
	private ProductExtrasDTO convertEntityToDto(final TempCartExtras tempCartExtras) {
		LOGGER.info("Inside convert Entity To Dto method");
		ProductExtrasDTO productExtrasDto = productExtrasMapper.toDto(tempCartExtras.getProductExtras());
		productExtrasDto.setQuantity(tempCartExtras.getQuantity());
		return productExtrasDto;
	}

	/**
	 * @param tempCartExtrasList
	 */
	private List<ProductExtrasDTO> convertEntityToDtos(final List<TempCartExtras> tempCartExtrasList) {
		List<ProductExtrasDTO> productExtrasDtoList = new ArrayList<>();
		for (TempCartExtras tempCartExtras : tempCartExtrasList) {
			productExtrasDtoList.add(convertEntityToDto(tempCartExtras));
		}
		return productExtrasDtoList;
	}

	/**
	 * @param tempCartItem
	 * @param tempCartExtras
	 */
	private boolean checkIfExistsTempCartExtrasForCartItemAndExtras(final TempCartItem tempCartItem, final ProductExtras productExtras) {
		return tempCartExtrasRepository.findAllByTempCartItemAndProductExtras(tempCartItem, productExtras).isPresent();

	}
}
