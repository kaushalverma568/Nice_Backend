package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.ProductAddonsDTO;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.ProductExtrasDTO;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.TempCartAddonsDTO;
import com.nice.dto.TempCartExtrasDto;
import com.nice.dto.TempCartItemDTO;
import com.nice.dto.TempCartProductAttributeValueDTO;
import com.nice.dto.TempCartToppingsDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.TempCartItemMapper;
import com.nice.model.ProductVariant;
import com.nice.model.TempCartItem;
import com.nice.repository.TempCartItemRepository;
import com.nice.service.ProductVariantService;
import com.nice.service.TempCartAddonsService;
import com.nice.service.TempCartExtrasService;
import com.nice.service.TempCartItemService;
import com.nice.service.TempCartProductAttributeValueService;
import com.nice.service.TempCartToppingsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("tempCartItemService")
public class TempCartItemServiceImpl implements TempCartItemService {
	private static final Logger LOGGER = LoggerFactory.getLogger(TempCartItemServiceImpl.class);
	private static final String INVALID_QTY = "invalid.quantity";
	@Autowired
	private TempCartItemRepository cartItemRepository;

	@Autowired
	private TempCartItemMapper cartItemMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private TempCartAddonsService tempCartAddonsService;

	@Autowired
	private TempCartProductAttributeValueService tempCartProductAttributeValueService;

	@Autowired
	private TempCartExtrasService tempCartExtrasService;

	@Autowired
	private TempCartToppingsService tempCartToppingsService;

	@Override
	public Long addTempCartItem(final TempCartItemDTO tempCartItemDTO) throws ValidationException, NotFoundException {
		/**
		 * Get Existing cartItem based on uuid
		 */

		List<TempCartItem> tempCartItemList = getCartListBasedOnUuid(tempCartItemDTO.getUuid());
		/**
		 * If the vendor For existing cartItem is different from the new product vendor delete the old cart and populate the new
		 * one
		 */
		if (!tempCartItemList.isEmpty()) {
			TempCartItem tempCartItem = tempCartItemList.get(0);
			ProductVariant productVariant = productVariantService.getProductVariantDetail(tempCartItemDTO.getProductVariantId());
			/**
			 * Delete existing cart if the vendor for the existing products in cart and new products are different
			 */
			if (!tempCartItem.getProductVariant().getVendorId().equals(productVariant.getVendorId())) {
				deleteTempCartItemForUuid(tempCartItemDTO.getUuid());
			}
		}

		TempCartItem tempCartItemEntity = cartItemMapper.toEntity(tempCartItemDTO);
		if (tempCartItemEntity.getQuantity() <= 0) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
		}
		ProductVariant productVariant = productVariantService.getProductVariantDetail(tempCartItemDTO.getProductVariantId());
		tempCartItemEntity.setProductVariant(productVariant);
		List<TempCartItem> existingTempCartItemList = getTempCartItemBasedOnUuidAndProductVariant(tempCartItemDTO.getUuid(), productVariant);
		if (!existingTempCartItemList.isEmpty()) {
			boolean productAlreadyAdded = false;
			for (TempCartItem tempCartItem : existingTempCartItemList) {
				/**
				 * Check if all Addons are same
				 */
				List<ProductAddonsDTO> tempCartAddonsList = tempCartAddonsService.getTempCartAddonsListForCartItem(tempCartItem.getId());
				List<Long> existingProductAddonsList = tempCartAddonsList.isEmpty() ? null
						: tempCartAddonsList.stream().map(ProductAddonsDTO::getId).collect(Collectors.toList());
				boolean allAddonsSame = false;
				if ((existingProductAddonsList == null && tempCartItemDTO.getProductAddonsId() == null) || (tempCartItemDTO.getProductAddonsId() != null
						&& existingProductAddonsList != null && existingProductAddonsList.size() == tempCartItemDTO.getProductAddonsId().size()
						&& existingProductAddonsList.containsAll(tempCartItemDTO.getProductAddonsId()))) {
					allAddonsSame = true;
				}

				/**
				 * Check if all Topppings are same
				 */
				List<ProductToppingResponseDTO> tempCartToppingsList = tempCartToppingsService.getTempCartToppingsListForCartItem(tempCartItem.getId());
				List<Long> existingProductToppingsList = tempCartToppingsList.isEmpty() ? null
						: tempCartToppingsList.stream().map(ProductToppingResponseDTO::getId).collect(Collectors.toList());
				boolean allToppingsSame = false;
				if ((existingProductToppingsList == null && tempCartItemDTO.getProductToppingsIds() == null) || (tempCartItemDTO.getProductToppingsIds() != null
						&& existingProductToppingsList != null && existingProductToppingsList.size() == tempCartItemDTO.getProductToppingsIds().size()
						&& existingProductToppingsList.containsAll(tempCartItemDTO.getProductToppingsIds()))) {
					allToppingsSame = true;
				}

				/**
				 * Check if all ProductAttributes are same
				 */
				List<ProductAttributeValueDTO> productAttributeValueDtoList = tempCartProductAttributeValueService
						.getTempCartProductAttributeValueListForCartItem(tempCartItem.getId());
				List<Long> existingProductAttributeValueDtoList = productAttributeValueDtoList.isEmpty() ? null
						: productAttributeValueDtoList.stream().map(ProductAttributeValueDTO::getId).collect(Collectors.toList());
				boolean allProductAttributeValuesSame = false;
				if ((existingProductAttributeValueDtoList == null && tempCartItemDTO.getAttributeValueIds() == null)
						|| (tempCartItemDTO.getAttributeValueIds() != null && existingProductAttributeValueDtoList != null
								&& existingProductAttributeValueDtoList.size() == tempCartItemDTO.getAttributeValueIds().size()
								&& existingProductAttributeValueDtoList.containsAll(tempCartItemDTO.getAttributeValueIds()))) {
					allProductAttributeValuesSame = true;
				}

				/**
				 * Check if all Extras are same
				 */
				List<ProductExtrasDTO> tempCartExtrasList = tempCartExtrasService.getTempCartExtrasListForCartItem(tempCartItem.getId());
				List<Long> existingProductExtrasList = tempCartExtrasList.isEmpty() ? null
						: tempCartExtrasList.stream().map(ProductExtrasDTO::getId).collect(Collectors.toList());
				boolean allExtrasSame = false;
				if ((existingProductExtrasList == null && tempCartItemDTO.getProductExtrasId() == null) || (tempCartItemDTO.getProductExtrasId() != null
						&& existingProductExtrasList != null && existingProductExtrasList.size() == tempCartItemDTO.getProductExtrasId().size()
						&& existingProductExtrasList.containsAll(tempCartItemDTO.getProductExtrasId()))) {
					allExtrasSame = true;
				}

				if (allAddonsSame && allToppingsSame && allProductAttributeValuesSame && allExtrasSame) {
					/**
					 * update cart item quantity by adding new quantity in previous quantity if total of existing and new is greater then 15
					 * , then set quantity as 15
					 **/
					updateTempCartItemQty(tempCartItem.getId(), tempCartItem.getQuantity() + tempCartItemEntity.getQuantity() > 15 ? 15
							: tempCartItem.getQuantity() + tempCartItemEntity.getQuantity());
					productAlreadyAdded = true;
					break;
				}
			}
			if (!productAlreadyAdded) {
				saveItemsToCart(tempCartItemEntity, tempCartItemDTO);
			}

		} else {
			if (tempCartItemEntity.getQuantity() > 15) {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
			}
			saveItemsToCart(tempCartItemEntity, tempCartItemDTO);
		}
		return tempCartItemEntity.getId();
	}

	/**
	 * @param  tempCartItemEntity
	 * @param  tempCartItemDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void saveItemsToCart(TempCartItem tempCartItemEntity, final TempCartItemDTO tempCartItemDTO) throws ValidationException, NotFoundException {
		tempCartItemEntity = cartItemRepository.save(tempCartItemEntity);
		if (tempCartItemDTO.getProductToppingsIds() != null && !tempCartItemDTO.getProductToppingsIds().isEmpty()) {
			for (Long id : tempCartItemDTO.getProductToppingsIds()) {
				TempCartToppingsDto tempCartToppingsDto = new TempCartToppingsDto(id, tempCartItemEntity.getId(), tempCartItemEntity.getQuantity(), true);
				tempCartToppingsService.addTempCartToppings(tempCartToppingsDto, tempCartItemEntity);
			}
		}

		if (tempCartItemDTO.getProductAddonsId() != null && !tempCartItemDTO.getProductAddonsId().isEmpty()) {
			for (Long id : tempCartItemDTO.getProductAddonsId()) {
				TempCartAddonsDTO tempCartAddonsDto = new TempCartAddonsDTO(id, tempCartItemEntity.getId(), tempCartItemEntity.getQuantity(), true);
				tempCartAddonsService.addTempCartAddons(tempCartAddonsDto, tempCartItemEntity);
			}
		}

		if (tempCartItemDTO.getAttributeValueIds() != null && !tempCartItemDTO.getAttributeValueIds().isEmpty()) {
			for (Long id : tempCartItemDTO.getAttributeValueIds()) {
				TempCartProductAttributeValueDTO tempCartProductAttributeValuesDto = new TempCartProductAttributeValueDTO(id, tempCartItemEntity.getId(),
						tempCartItemEntity.getQuantity(), true);
				tempCartProductAttributeValueService.addTempCartProductAttributeValue(tempCartProductAttributeValuesDto, tempCartItemEntity);
			}
		}

		if (tempCartItemDTO.getProductExtrasId() != null && !tempCartItemDTO.getProductExtrasId().isEmpty()) {
			for (Long id : tempCartItemDTO.getProductExtrasId()) {
				TempCartExtrasDto tempCartExtrasDto = new TempCartExtrasDto(id, tempCartItemEntity.getId(), tempCartItemEntity.getQuantity(), true);
				tempCartExtrasService.addTempCartExtras(tempCartExtrasDto, tempCartItemEntity);
			}
		}

	}

	@Override
	public List<TempCartItem> getTempCartItemBasedOnUuidAndProductVariant(final String uuid, final ProductVariant productVariant) {
		return cartItemRepository.findAllByUuidAndProductVariant(uuid, productVariant);
	}

	@Override
	public void updateTempCartItemQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		if (quantity <= 0 || quantity > 15) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
		} else {
			final TempCartItem cartItem = getTempCartItemDetail(cartItemId);
			cartItem.setQuantity(quantity);
			cartItemRepository.save(cartItem);
			tempCartAddonsService.updateTempCartAddonsQty(cartItemId, quantity);
			tempCartExtrasService.updateTempCartExtrasQty(cartItemId, quantity);
			tempCartToppingsService.updateTempCartToppingsQty(cartItemId, quantity);
			tempCartProductAttributeValueService.updateTempCartProductAttributeValueQty(cartItemId, quantity);
		}
	}

	@Override
	public TempCartItem getTempCartItemDetail(final Long cartItemId) throws NotFoundException {
		return cartItemRepository.findById(cartItemId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("cart.item.not.found.id", new Object[] { cartItemId })));
	}

	private CartItemResponseDTO convertEntityToResponseDto(final TempCartItem cartItem) throws NotFoundException, ValidationException {
		LOGGER.info("Inside convert Entity To ResponseDto method");
		CartItemResponseDTO cartItemResponseDTO = cartItemMapper.toDto(cartItem);
		ProductVariantResponseDTO productVariantResponseDto = productVariantService.getProductVariantInternal(cartItem.getProductVariant().getId(), false);
		cartItemResponseDTO.setProductVariantResponseDto(productVariantResponseDto);
		cartItemResponseDTO.setProductAddonsDtoList(tempCartAddonsService.getTempCartAddonsListForCartItem(cartItem.getId()));
		cartItemResponseDTO.setProductToppingsDtoList(tempCartToppingsService.getTempCartToppingsListForCartItem(cartItem.getId()));
		cartItemResponseDTO.setProductExtrasDtoList(tempCartExtrasService.getTempCartExtrasListForCartItem(cartItem.getId()));

		List<ProductAttributeValueDTO> productAttributeValueDtoList = tempCartProductAttributeValueService
				.getTempCartProductAttributeValueListForCartItem(cartItem.getId());
		Map<String, List<ProductAttributeValueDTO>> productAttributeValueDtoMap = new HashMap<>();
		for (ProductAttributeValueDTO productAttributeValueDTO : productAttributeValueDtoList) {
			if (productAttributeValueDtoMap.get(productAttributeValueDTO.getProductAttributeName()) == null) {
				List<ProductAttributeValueDTO> productAttributeValueDTOList = new ArrayList<>();
				productAttributeValueDTOList.add(productAttributeValueDTO);
				productAttributeValueDtoMap.put(productAttributeValueDTO.getProductAttributeName(), productAttributeValueDTOList);
			} else {
				productAttributeValueDtoMap.get(productAttributeValueDTO.getProductAttributeName()).add(productAttributeValueDTO);
			}
		}
		cartItemResponseDTO.setProductAttributeValuesDtoMap(productAttributeValueDtoMap);
		return cartItemResponseDTO;
	}

	@Override
	public void addTempCartItemList(final List<TempCartItemDTO> cartItemDTOs) throws NotFoundException, ValidationException {
		for (TempCartItemDTO cartItemDTO : cartItemDTOs) {
			/**
			 * add cart item
			 */
			addTempCartItem(cartItemDTO);
		}
	}

	@Override
	public void deleteTempCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside delete Cart Item method {}", cartItemId);
		tempCartAddonsService.deleteTempCartAddons(cartItemId);
		tempCartExtrasService.deleteTempCartExtras(cartItemId);
		tempCartToppingsService.deleteTempCartToppings(cartItemId);
		tempCartProductAttributeValueService.deleteTempCartProductAttributeValue(cartItemId);
		cartItemRepository.deleteById(cartItemId);
	}

	@Override
	public Long getTempCartItemCountForUuid(final String uuid) throws ValidationException {
		if (uuid == null) {
			LOGGER.error("uuid is null");
			throw new ValidationException(messageByLocaleService.getMessage("cart.uuid.not.null", null));
		} else {
			return cartItemRepository.countByUuid(uuid);
		}
	}

	@Override
	public List<CartItemResponseDTO> getTempCartItemDetailListByParam(final String uuid) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Cart Item List By Param method {}", uuid);
		final List<CartItemResponseDTO> cartItemResponseDTOs = new ArrayList<>();
		final List<TempCartItem> cartItems = getCartListBasedOnUuid(uuid);
		for (TempCartItem cartItem : cartItems) {
			cartItemResponseDTOs.add(convertEntityToResponseDto(cartItem));
		}
		return cartItemResponseDTOs;
	}

	@Override
	public List<TempCartItem> getCartListBasedOnUuid(final String uuid) throws ValidationException {
		final List<TempCartItem> cartItems;
		if (uuid == null) {
			LOGGER.error("uuid is null");
			throw new ValidationException(messageByLocaleService.getMessage("cart.uuid.not.null", null));
		} else {
			cartItems = cartItemRepository.findAllByUuid(uuid);
		}
		return cartItems;
	}

	@Override
	public void deleteTempCartItemForUuid(final String uuid) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete Cart Item method for customer : {}", uuid);
		List<TempCartItem> cartItemList = getCartListBasedOnUuid(uuid);
		for (TempCartItem cartItem : cartItemList) {
			deleteTempCartItem(cartItem.getId());
		}
	}

	@Override
	public void deleteCartItemsForProductVariant(final Long productVariantId) throws NotFoundException {
		cartItemRepository.deleteAllByProductVariantId(productVariantId);
	}

	@Override
	public Boolean checkIfExistsCartItemWithDifferentVendor(final String uuid, final Long vendorId) throws ValidationException {

		List<TempCartItem> tempCartItemList = getCartListBasedOnUuid(uuid);
		return (!tempCartItemList.isEmpty() && !vendorId.equals(tempCartItemList.get(0).getProductVariant().getVendorId()));
	}
}
