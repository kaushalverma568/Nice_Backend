/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ProductToppingDto;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.CartToppings;
import com.nice.model.ProductTopping;
import com.nice.model.ProductVariant;
import com.nice.model.Topping;
import com.nice.model.UserLogin;
import com.nice.repository.ProductToppingRepository;
import com.nice.service.CartToppingsService;
import com.nice.service.ProductToppingService;
import com.nice.service.ProductVariantService;
import com.nice.service.ToppingService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service("productToppingService")
@Transactional(rollbackFor = Throwable.class)
public class ProductToppingServiceImpl implements ProductToppingService {

	/**
	 *
	 */
	@Autowired
	private ProductToppingRepository productToppingRepository;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private CartToppingsService cartToppingsService;

	@Autowired
	private ToppingService toppingService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductToppingServiceImpl.class);

	@Override
	public void addUpdateProductTopping(@Valid final List<ProductToppingDto> productToppingDTOList, final Long productVariantId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside addUpdateProductTopping method, with productVariantId : {}", productVariantId);
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		/**
		 * check of the vendor of product vendor is same as the one creating the topping
		 */
		if (!productVariant.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		for (ProductToppingDto productToppingDto : productToppingDTOList) {
			Topping topping = validateProductTopping(productVariant, productToppingDto);
			productToppingDto.setProductVariantId(productVariantId);
			ProductTopping productTopping = new ProductTopping();
			if (productToppingDto.getId() != null) {
				productTopping = getProductToppingDetails(productToppingDto.getId());
				if (!productTopping.getProductVariant().getId().equals(productVariant.getId())) {
					throw new ValidationException(messageByLocaleService.getMessage("topping.associated.to.variant", null));
				}
			}
			BeanUtils.copyProperties(productToppingDto, productTopping);
			productTopping.setProductVariant(productVariant);
			productTopping.setVendorId(vendorId);
			productTopping.setTopping(topping);
			productToppingRepository.save(productTopping);
		}
		LOGGER.info("After addUpdateProductTopping method, with productVariantId : {}", productVariantId);
	}

	@Override
	public ProductToppingResponseDTO getProductTopping(final Long productToppingId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside getProductTopping method, with productToppingId : {}", productToppingId);
		ProductTopping productTopping = getProductToppingDetails(productToppingId);
		UserLogin userLogin = checkForUserLogin();
		if (userLogin.getEntityType() == null || userLogin.getEntityId().equals(productTopping.getVendorId())) {
			return convertFromEntityToDto(productTopping);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
	}

	@Override
	public ProductToppingResponseDTO convertFromEntityToDto(final ProductTopping productTopping) {
		LOGGER.info("Inside convertFromEntityToDto");
		ProductToppingResponseDTO productToppingResponseDTO = new ProductToppingResponseDTO();
		BeanUtils.copyProperties(productTopping, productToppingResponseDTO);
		productToppingResponseDTO.setProductVariantId(productTopping.getProductVariant().getId());
		productToppingResponseDTO.setName(productTopping.getTopping().getName());
		productToppingResponseDTO.setDescription(productTopping.getTopping().getDescription());
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(productTopping.getTopping().getProductFoodType())) {
			productToppingResponseDTO.setProductFoodType(productTopping.getTopping().getProductFoodType());
		}
		return productToppingResponseDTO;
	}

	@Override
	public List<ProductToppingResponseDTO> getDtoListWithUserCheck(Boolean activeRecords, final Long productVariantId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside getDtoListWithUserCheck , active :{}, productVariantId : {}", activeRecords, productVariantId);
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * If the userLogin is null or userType is customer show only activeRecords
		 * irrespective of what is sent from front end.
		 */
		if (userLogin != null && (UserType.VENDOR.name().equals(userLogin.getEntityType()) && !productVariant.getVendorId().equals(userLogin.getEntityId()))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else if (userLogin == null || UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			activeRecords = true;
		}
		return getToppingForProductVariant(productVariantId, activeRecords);
	}

	@Override
	public List<ProductToppingResponseDTO> getToppingForProductVariant(final Long productVariantId, final Boolean active) {
		LOGGER.info("Inside getToppingForProductVariant , active :{}, productVariantId : {}", active, productVariantId);
		List<ProductTopping> productToppingList = null;
		if (active == null) {
			productToppingList = productToppingRepository.findAllByProductVariantId(productVariantId);
		} else {
			productToppingList = productToppingRepository.findAllByProductVariantIdAndActive(productVariantId, active);
		}
		return convertEntityListToDtos(productToppingList);
	}

	@Override
	public List<ProductTopping> getProductToppingListForTopping(final Topping topping, final Boolean active) {
		LOGGER.info("Inside getToppingForProductVariant , active :{}, toppingId : {}", active, topping.getId());
		if (active == null) {
			return productToppingRepository.findAllByTopping(topping);
		} else {
			return productToppingRepository.findAllByToppingAndActive(topping, active);
		}
	}

	/**
	 * @param productToppingList
	 * @return
	 */
	@Override
	public List<ProductToppingResponseDTO> convertEntityListToDtos(final List<ProductTopping> productToppingList) {
		LOGGER.info("Inside convertEntityListToDtos");
		List<ProductToppingResponseDTO> productToppingDtos = new ArrayList<>();
		for (ProductTopping productTopping : productToppingList) {
			productToppingDtos.add(convertFromEntityToDto(productTopping));
		}
		LOGGER.info("After convertEntityListToDtos");
		return productToppingDtos;
	}

	@Override
	public void changeStatus(final Long productToppingId, final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside changeStatus method, productToppingId :{} and active ;{}", productToppingId, active);
		ProductTopping productTopping = getProductToppingDetails(productToppingId);
		Long vendorId = getVendorIdForLoginUser();
		if (!productTopping.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (productTopping.getActive().equals(active)) {
			if (Boolean.TRUE.equals(active)) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.already.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("topping.already.deactive", null));
			}

		} else {
			if (Boolean.TRUE.equals(active)) {
				if (Boolean.FALSE.equals(productTopping.getProductVariant().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
				}
			} else {
				/**
				 * remove this topping from cart topping
				 */
				List<CartToppings> cartToppingList = cartToppingsService.getCartToppingsListBasedOnProductTopping(productTopping);
				for (CartToppings cartToppings : cartToppingList) {
					cartToppingsService.deleteCartToppingsById(cartToppings.getId());
				}
			}
			productTopping.setActive(active);
			productToppingRepository.save(productTopping);
		}
		LOGGER.info("After changeStatus method, productToppingId :{} and active ;{}", productToppingId, active);
	}

	@Override
	public ProductTopping getProductToppingDetails(final Long productToppingId) throws NotFoundException {
		LOGGER.info("Inside getProductToppingDetails method, productToppingId :{} ", productToppingId);
		return productToppingRepository.findById(productToppingId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("topping.not.found", new Object[] { productToppingId })));
	}

	/**
	 *
	 */
	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
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

	private Topping validateProductTopping(final ProductVariant productVariant, final ProductToppingDto productToppingDto)
			throws ValidationException, NotFoundException {
		LOGGER.info("Inside validateProductTopping method ");
		validateDTOProperties(productToppingDto);
		Long vendorId = getVendorIdForLoginUser();
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productToppingDto.getActive().booleanValue() && Boolean.FALSE.equals(productVariant.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
		} else if (productToppingDto.getToppingId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.id.not.null", null));
		}
		Topping topping = toppingService.getToppingDetail(productToppingDto.getToppingId());
		if (productToppingDto.getId() != null) {
			if (productToppingRepository.findByProductVariantAndToppingAndIdNot(productVariant, topping, productToppingDto.getId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		} else {
			if (productToppingRepository.findByProductVariantAndTopping(productVariant, topping).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		}
		LOGGER.info("After validateProductTopping method ");
		return topping;
	}

	private void validateDTOProperties(final ProductToppingDto productToppingDto) throws ValidationException {
		LOGGER.info("Inside validateDTOProperties method ");
		if (productToppingDto.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.rate.not.null", null));
		} else if (productToppingDto.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		}
		LOGGER.info("After validateDTOProperties method ");
	}

}
