package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.CartAddonsDTO;
import com.nice.dto.CartExtrasDto;
import com.nice.dto.CartItemDTO;
import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.CartProductAttributeValueDTO;
import com.nice.dto.CartToppingsDto;
import com.nice.dto.ProductAddonsDTO;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.ProductExtrasDTO;
import com.nice.dto.ProductToppingDto;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CartItemMapper;
import com.nice.model.CartItem;
import com.nice.model.Customer;
import com.nice.model.ProductVariant;
import com.nice.model.UserLogin;
import com.nice.repository.CartItemRepository;
import com.nice.service.CartAddonsService;
import com.nice.service.CartExtrasService;
import com.nice.service.CartItemService;
import com.nice.service.CartProductAttributeValueService;
import com.nice.service.CartToppingsService;
import com.nice.service.CustomerService;
import com.nice.service.ProductVariantService;
import com.nice.service.TempCartItemService;
import com.nice.util.CommonUtility;

@Transactional(rollbackFor = Throwable.class)
@Service("cartItemService")
public class CartItemServiceImpl implements CartItemService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CartItemServiceImpl.class);
	private static final String INVALID_QTY = "invalid.quantity";
	@Autowired
	private CartItemRepository cartItemRepository;

	@Autowired
	private CartItemMapper cartItemMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private CartAddonsService cartAddonsService;

	@Autowired
	private CartProductAttributeValueService cartProductAttributeValueService;

	@Autowired
	private CartExtrasService cartExtrasService;

	@Autowired
	private CartToppingsService cartToppingsService;

	@Autowired
	private CustomerService customerService;

	@Autowired
	private TempCartItemService tempCartItemService;

	@Override
	public Long addCartItem(final CartItemDTO cartItemDTO) throws ValidationException, NotFoundException {
		/**
		 * Get Existing cartItem based on customerId
		 */
		cartItemDTO.setCustomerId(getCustomerIdForLoginUser());
		List<CartItem> cartItemList = getCartListBasedOnCustomer(cartItemDTO.getCustomerId());
		Customer customer = customerService.getCustomerDetails(cartItemDTO.getCustomerId());
		/**
		 * If the vendor For existing cartItem is different from the new product vendor
		 * delete the old cart and populate the new one
		 */
		if (!cartItemList.isEmpty()) {
			CartItem cartItem = cartItemList.get(0);
			ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItemDTO.getProductVariantId());
			/**
			 * Delete existing cart if the vendor for the existing products in cart and new
			 * products are different
			 */
			if (!cartItem.getProductVariant().getVendorId().equals(productVariant.getVendorId())) {
				deleteCartItemForCustomer(customer.getId());
			}
		}

		CartItem cartItemEntity = cartItemMapper.toEntity(cartItemDTO);
		if (cartItemEntity.getQuantity() <= 0) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
		}
		ProductVariant productVariant = productVariantService.getProductVariantDetail(cartItemDTO.getProductVariantId());
		cartItemEntity.setProductVariant(productVariant);
		cartItemEntity.setCustomer(customer);
		List<CartItem> existingCartItemList = getCartItemBasedOnCustomerAndProductVariant(customer, productVariant);
		if (!existingCartItemList.isEmpty()) {
			boolean productAlreadyAdded = false;
			for (CartItem cartItem : existingCartItemList) {
				/**
				 * Check if all Addons are same
				 */
				List<ProductAddonsDTO> cartAddonsList = cartAddonsService.getCartAddonsDtoListForCartItem(cartItem.getId());
				List<Long> existingProductAddonsList = cartAddonsList.isEmpty() ? null
						: cartAddonsList.stream().map(ProductAddonsDTO::getId).collect(Collectors.toList());
				boolean allAddonsSame = false;
				if ((existingProductAddonsList == null && cartItemDTO.getProductAddonsId() == null) || (cartItemDTO.getProductAddonsId() != null
						&& existingProductAddonsList != null && existingProductAddonsList.size() == cartItemDTO.getProductAddonsId().size()
						&& existingProductAddonsList.containsAll(cartItemDTO.getProductAddonsId()))) {
					allAddonsSame = true;
				}

				/**
				 * Check if all Topppings are same
				 */
				List<ProductToppingDto> cartToppingsList = cartToppingsService.getProductToppingsDtoListForCartItem(cartItem.getId());
				List<Long> existingProductToppingsList = cartToppingsList.isEmpty() ? null
						: cartToppingsList.stream().map(ProductToppingDto::getId).collect(Collectors.toList());
				boolean allToppingsSame = false;
				if ((existingProductToppingsList == null && cartItemDTO.getProductToppingsIds() == null) || (cartItemDTO.getProductToppingsIds() != null
						&& existingProductToppingsList != null && existingProductToppingsList.size() == cartItemDTO.getProductToppingsIds().size()
						&& existingProductToppingsList.containsAll(cartItemDTO.getProductToppingsIds()))) {
					allToppingsSame = true;
				}

				/**
				 * Check if all ProductAttributes are same
				 */
				List<ProductAttributeValueDTO> productAttributeValueDtoList = cartProductAttributeValueService
						.getProductAttributeValueDtoListForCartItem(cartItem.getId());
				List<Long> existingProductAttributeValueDtoList = productAttributeValueDtoList.isEmpty() ? null
						: productAttributeValueDtoList.stream().map(ProductAttributeValueDTO::getId).collect(Collectors.toList());
				boolean allProductAttributeValuesSame = false;
				if ((existingProductAttributeValueDtoList == null && cartItemDTO.getAttributeValueIds() == null)
						|| (cartItemDTO.getAttributeValueIds() != null && existingProductAttributeValueDtoList != null
								&& existingProductAttributeValueDtoList.size() == cartItemDTO.getAttributeValueIds().size()
								&& existingProductAttributeValueDtoList.containsAll(cartItemDTO.getAttributeValueIds()))) {
					allProductAttributeValuesSame = true;
				}

				/**
				 * Check if all Extras are same
				 */
				List<ProductExtrasDTO> cartExtrasList = cartExtrasService.getCartExtrasDtoListForCartItem(cartItem.getId());
				List<Long> existingProductExtrasList = cartExtrasList.isEmpty() ? null
						: cartExtrasList.stream().map(ProductExtrasDTO::getId).collect(Collectors.toList());
				boolean allExtrasSame = false;
				if ((existingProductExtrasList == null && cartItemDTO.getProductExtrasId() == null) || (cartItemDTO.getProductExtrasId() != null
						&& existingProductExtrasList != null && existingProductExtrasList.size() == cartItemDTO.getProductExtrasId().size()
						&& existingProductExtrasList.containsAll(cartItemDTO.getProductExtrasId()))) {
					allExtrasSame = true;
				}

				if (allAddonsSame && allToppingsSame && allProductAttributeValuesSame && allExtrasSame) {
					/**
					 * update cart item quantity by adding new quantity in previous quantity if
					 * total of existing and new is greater then 15 , then set quantity as 15
					 **/
					updateCartItemQty(cartItem.getId(),
							cartItem.getQuantity() + cartItemEntity.getQuantity() > 15 ? 15 : cartItem.getQuantity() + cartItemEntity.getQuantity());
					productAlreadyAdded = true;
					break;
				}
			}
			if (!productAlreadyAdded) {
				saveItemsToCart(cartItemEntity, cartItemDTO);
			}

		} else {
			if (cartItemEntity.getQuantity() > 15) {
				throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
			}
			saveItemsToCart(cartItemEntity, cartItemDTO);
		}
		return cartItemEntity.getId();
	}

	@Override
	public void deleteCartItemForCustomer(final Long id) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete Cart Item method {}", id);
		List<CartItem> cartItemList = getCartListBasedOnCustomer(id);
		for (CartItem cartItem : cartItemList) {
			deleteCartItem(cartItem.getId());
		}
	}

	/**
	 * @param cartItemEntity
	 * @param cartItemDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void saveItemsToCart(CartItem cartItemEntity, final CartItemDTO cartItemDTO) throws ValidationException, NotFoundException {
		cartItemEntity = cartItemRepository.save(cartItemEntity);
		if (cartItemDTO.getProductToppingsIds() != null && !cartItemDTO.getProductToppingsIds().isEmpty()) {
			for (Long id : cartItemDTO.getProductToppingsIds()) {
				CartToppingsDto cartToppingsDto = new CartToppingsDto(id, cartItemEntity.getId(), cartItemEntity.getQuantity(), true);
				cartToppingsService.addCartToppings(cartToppingsDto, cartItemEntity);
			}
		}

		if (cartItemDTO.getProductAddonsId() != null && !cartItemDTO.getProductAddonsId().isEmpty()) {
			for (Long id : cartItemDTO.getProductAddonsId()) {
				CartAddonsDTO cartAddonsDto = new CartAddonsDTO(id, cartItemEntity.getId(), cartItemEntity.getQuantity(), true);
				cartAddonsService.addCartAddons(cartAddonsDto, cartItemEntity);
			}
		}

		if (cartItemDTO.getAttributeValueIds() != null && !cartItemDTO.getAttributeValueIds().isEmpty()) {
			for (Long id : cartItemDTO.getAttributeValueIds()) {
				CartProductAttributeValueDTO cartProductAttributeValuesDto = new CartProductAttributeValueDTO(id, cartItemEntity.getId(),
						cartItemEntity.getQuantity(), true);
				cartProductAttributeValueService.addCartProductAttributeValue(cartProductAttributeValuesDto, cartItemEntity);
			}
		}

		if (cartItemDTO.getProductExtrasId() != null && !cartItemDTO.getProductExtrasId().isEmpty()) {
			for (Long id : cartItemDTO.getProductExtrasId()) {
				CartExtrasDto cartExtrasDto = new CartExtrasDto(id, cartItemEntity.getId(), cartItemEntity.getQuantity(), true);
				cartExtrasService.addCartExtras(cartExtrasDto, cartItemEntity);
			}
		}

	}

	@Override
	public List<CartItem> getCartItemBasedOnCustomerAndProductVariant(final Customer customer, final ProductVariant productVariant) {
		return cartItemRepository.findAllByCustomerAndProductVariant(customer, productVariant);
	}

	@Override
	public void updateCartItemQty(final Long cartItemId, final Long quantity) throws NotFoundException, ValidationException {
		Long customerId = getCustomerIdForLoginUser();
		if (quantity <= 0 || quantity > 15) {
			throw new ValidationException(messageByLocaleService.getMessage(INVALID_QTY, null));
		} else {
			final CartItem cartItem = getCartItemDetail(cartItemId);
			if (!customerId.equals(cartItem.getCustomer().getId())) {
				throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
			}
			cartItem.setQuantity(quantity);
			cartItemRepository.save(cartItem);
			cartAddonsService.updateCartAddonsQty(cartItemId, quantity);
			cartExtrasService.updateCartExtrasQty(cartItemId, quantity);
			cartToppingsService.updateCartToppingsQty(cartItemId, quantity);
			cartProductAttributeValueService.updateCartProductAttributeValueQty(cartItemId, quantity);
		}
	}

	@Override
	public CartItem getCartItemDetail(final Long cartItemId) throws NotFoundException {
		return cartItemRepository.findById(cartItemId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("cart.item.not.found.id", new Object[] { cartItemId })));
	}

	@Override
	public CartItemResponseDTO getCartItem(final Long cartItemId, final Long pincodeId) throws NotFoundException, ValidationException {
		final CartItem cartItem = getCartItemDetail(cartItemId);
		return convertEntityToResponseDto(cartItem);
	}

	private CartItemResponseDTO convertEntityToResponseDto(final CartItem cartItem) throws NotFoundException, ValidationException {
		LOGGER.info("Inside convert Entity To ResponseDto method");
		CartItemResponseDTO cartItemResponseDTO = cartItemMapper.toDto(cartItem);
		ProductVariantResponseDTO productVariantResponseDto = productVariantService.getProductVariantInternal(cartItem.getProductVariant().getId(), false);
		cartItemResponseDTO.setProductVariantResponseDto(productVariantResponseDto);
		cartItemResponseDTO.setProductAddonsDtoList(cartAddonsService.getCartAddonsDtoListForCartItem(cartItem.getId()));
		cartItemResponseDTO.setProductToppingsDtoList(cartToppingsService.getProductToppingsDtoListForCartItem(cartItem.getId()));
		cartItemResponseDTO.setProductExtrasDtoList(cartExtrasService.getCartExtrasDtoListForCartItem(cartItem.getId()));
		cartItemResponseDTO.setCustomerId(cartItem.getCustomer().getId());
		List<ProductAttributeValueDTO> productAttributeValueDtoList = cartProductAttributeValueService
				.getProductAttributeValueDtoListForCartItem(cartItem.getId());
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
	public void addCartItemList(final List<CartItemDTO> cartItemDTOs) throws NotFoundException, ValidationException {
		for (CartItemDTO cartItemDTO : cartItemDTOs) {
			/**
			 * add cart item
			 */
			addCartItem(cartItemDTO);
		}
	}

	@Override
	public void deleteCartItem(final Long cartItemId) throws NotFoundException {
		LOGGER.info("Inside delete Cart Item method {}", cartItemId);
		cartAddonsService.deleteCartAddons(cartItemId);
		cartExtrasService.deleteCartExtras(cartItemId);
		cartToppingsService.deleteCartToppings(cartItemId);
		cartProductAttributeValueService.deleteCartProductAttributeValue(cartItemId);
		cartItemRepository.deleteById(cartItemId);
	}

	@Override
	public Long getCartItemCount() throws ValidationException, NotFoundException {
		UserLogin userLogin = getUserLoginFromToken();
		if (!UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			LOGGER.error("customerId is null");
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			Customer customer = customerService.getCustomerDetails(userLogin.getEntityId());
			return cartItemRepository.countByCustomer(customer);
		}
	}

	@Override
	public List<CartItemResponseDTO> getCartItemDetailList() throws NotFoundException, ValidationException {
		Long customerId = getCustomerIdForLoginUser();
		LOGGER.info("Inside get Cart Item List By customer {}", customerId);

		final List<CartItemResponseDTO> cartItemResponseDTOs = new ArrayList<>();
		final List<CartItem> cartItems = getCartListBasedOnCustomer(customerId);
		for (CartItem cartItem : cartItems) {
			cartItemResponseDTOs.add(convertEntityToResponseDto(cartItem));
		}
		return cartItemResponseDTOs;
	}

	@Override
	public List<CartItem> getCartListBasedOnCustomer(final Long customerId) throws ValidationException, NotFoundException {
		final List<CartItem> cartItems;

		if (customerId == null) {
			LOGGER.error("customer id is null");
			throw new ValidationException(messageByLocaleService.getMessage("cart.customer.id.not.null", null));
		} else {
			Customer customer = customerService.getCustomerDetails(customerId);
			cartItems = cartItemRepository.findAllByCustomer(customer);
		}
		return cartItems;
	}

	@Override
	public void deleteCart() throws NotFoundException, ValidationException {
		Long customerId = getCustomerIdForLoginUser();
		deleteCartItemForCustomer(customerId);
	}

	@Override
	public Boolean checkIfExistsCartItemWithDifferentVendor(final Long vendorId) throws ValidationException, NotFoundException {
		Long customerId = getCustomerIdForLoginUser();
		List<CartItem> cartItemList = getCartListBasedOnCustomer(customerId);
		return (!cartItemList.isEmpty() && !vendorId.equals(cartItemList.get(0).getProductVariant().getVendorId()));
	}

	private UserLogin getUserLoginFromToken() {
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (Constant.ANONYMOUS_USER.equals(principal)) {
			return null;
		}
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private UserLogin checkForUserLogin() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin == null) {
			throw new ValidationException(messageByLocaleService.getMessage("login.first", null));
		} else {
			return userLogin;
		}
	}

	/**
	 * @throws ValidationException
	 *
	 */
	private Long getCustomerIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	@Override
	public void moveFromTempCartToCart(final String uuid) throws NotFoundException, ValidationException {
		List<CartItemResponseDTO> cartItemResponseList = tempCartItemService.getTempCartItemDetailListByParam(uuid);
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(cartItemResponseList)) {
			throw new ValidationException(messageByLocaleService.getMessage("cannot.move.cart.empty", null));
		}
		List<CartItemDTO> cartItemDtoList = new ArrayList<>();
		for (CartItemResponseDTO cartItemResponseDTO : cartItemResponseList) {
			CartItemDTO cartItemDto = convertFromCartItemResponseToCartItemDto(cartItemResponseDTO);
			cartItemDtoList.add(cartItemDto);
		}
		addCartItemList(cartItemDtoList);
		tempCartItemService.deleteTempCartItemForUuid(uuid);
	}

	private CartItemDTO convertFromCartItemResponseToCartItemDto(final CartItemResponseDTO cartItemResponseDto) {
		CartItemDTO cartItemDto = new CartItemDTO();
		cartItemDto.setActive(true);
		cartItemDto.setQuantity(cartItemResponseDto.getQuantity());
		cartItemDto.setProductVariantId(cartItemResponseDto.getProductVariantResponseDto().getId());
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(cartItemResponseDto.getProductExtrasDtoList())) {
			List<Long> productExtrasList = cartItemResponseDto.getProductExtrasDtoList().stream().map(ProductExtrasDTO::getId).collect(Collectors.toList());
			cartItemDto.setProductExtrasId(productExtrasList);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(cartItemResponseDto.getProductAddonsDtoList())) {
			List<Long> productAddonsList = cartItemResponseDto.getProductAddonsDtoList().stream().map(ProductAddonsDTO::getId).collect(Collectors.toList());
			cartItemDto.setProductAddonsId(productAddonsList);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(cartItemResponseDto.getProductToppingsDtoList())) {
			List<Long> productToppingsList = cartItemResponseDto.getProductToppingsDtoList().stream().map(ProductToppingDto::getId)
					.collect(Collectors.toList());
			cartItemDto.setProductToppingsIds(productToppingsList);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_MAP.test(cartItemResponseDto.getProductAttributeValuesDtoMap())) {
			List<Long> attributeValueList = new ArrayList<>();
			for (List<ProductAttributeValueDTO> productAttributeValueList : cartItemResponseDto.getProductAttributeValuesDtoMap().values()) {
				List<Long> attValList = productAttributeValueList.stream().map(ProductAttributeValueDTO::getId).collect(Collectors.toList());
				attributeValueList.addAll(attValList);
			}
			cartItemDto.setAttributeValueIds(attributeValueList);
		}

		return cartItemDto;
	}

	@Override
	public void deleteCartItemsForProductVariant(final Long productVariantId) throws NotFoundException {
		cartItemRepository.deleteAllByProductVariantId(productVariantId);
	}
}
