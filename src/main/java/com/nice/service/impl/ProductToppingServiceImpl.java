/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.UserType;
import com.nice.dto.ProductToppingDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.ProductTopping;
import com.nice.model.ProductVariant;
import com.nice.model.UserLogin;
import com.nice.repository.ProductToppingRepository;
import com.nice.service.ProductToppingService;
import com.nice.service.ProductVariantService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service("productToppingServiceImpl")
@Transactional(rollbackFor = Throwable.class)
public class ProductToppingServiceImpl implements ProductToppingService {

	/**
	 *
	 */
	private static final String UNAUTHORIZED = "unauthorized";

	@Autowired
	private ProductToppingRepository productToppingRepository;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addUpdateProductTopping(@Valid final List<ProductToppingDto> productToppingDTOList, final Long productVariantId)
			throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		for (ProductToppingDto productToppingDto : productToppingDTOList) {
			validateProductTopping(productVariant, productToppingDto);
			productToppingDto.setProductVariantId(productVariantId);
			ProductTopping productTopping = new ProductTopping();
			if (productToppingDto.getId() != null) {
				productTopping = getProductToppingDetails(productToppingDto.getId());
				if (!productTopping.getVendorId().equals(vendorId)) {
					throw new ValidationException(messageByLocaleService.getMessage(UNAUTHORIZED, null));
				} else if (!productTopping.getProductVariant().getId().equals(productVariant.getId())) {
					throw new ValidationException(messageByLocaleService.getMessage("topping.associated.to.variant", null));
				}
			}
			BeanUtils.copyProperties(productToppingDto, productTopping);
			productTopping.setVendorId(vendorId);
			productTopping.setProductVariant(productVariant);
			productToppingRepository.save(productTopping);
		}

	}

	@Override
	public ProductToppingDto getProductTopping(final Long productToppingId) throws NotFoundException {
		ProductTopping productTopping = getProductToppingDetails(productToppingId);
		return convertFromEntityToDto(productTopping);
	}

	@Override
	public ProductToppingDto convertFromEntityToDto(final ProductTopping productTopping) {
		ProductToppingDto productToppingDto = new ProductToppingDto();
		BeanUtils.copyProperties(productTopping, productToppingDto);
		productToppingDto.setProductVariantId(productTopping.getProductVariant().getId());
		return productToppingDto;
	}

	@Override
	public List<ProductToppingDto> getToppingForProductVariant(final Long productVariantId, final Boolean active) {
		List<ProductTopping> productToppingList = null;
		if (active == null) {
			productToppingList = productToppingRepository.findAllByProductVariantId(productVariantId);
		} else {
			productToppingList = productToppingRepository.findAllByProductVariantIdAndActive(productVariantId, active);
		}

		List<ProductToppingDto> productToppingDtos = convertEntityListToDtos(productToppingList);
		return productToppingDtos;
	}

	/**
	 * @param productToppingList
	 * @return
	 */
	@Override
	public List<ProductToppingDto> convertEntityListToDtos(final List<ProductTopping> productToppingList) {
		List<ProductToppingDto> productToppingDtos = new ArrayList<>();
		for (ProductTopping productTopping : productToppingList) {
			productToppingDtos.add(convertFromEntityToDto(productTopping));
		}
		return productToppingDtos;
	}

	@Override
	public void changeStatus(final Long productToppingId, final Boolean active) throws NotFoundException, ValidationException {
		ProductTopping productTopping = getProductToppingDetails(productToppingId);
		UserLogin userLogin = getUserLoginFromToken();
		Long vendorId = getVendorIdForLoginUser();
		if (!(userLogin.getEntityType() == null || vendorId != null)) {
			throw new ValidationException(messageByLocaleService.getMessage(UNAUTHORIZED, null));
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
				// cartItemService.deleteCartItemsForProductVariant(productVariant.getId());
				// tempCartItemService.deleteCartItemsForProductVariant(productVariant.getId());
				// posCartService.deleteAllByProductVariant(productVariant.getId());
			}
			productTopping.setActive(active);
			productToppingRepository.save(productTopping);
		}
	}

	@Override
	public ProductTopping getProductToppingDetails(final Long productToppingId) throws NotFoundException {
		return productToppingRepository.findById(productToppingId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("topping.not.found", new Object[] { productToppingId })));
	}

	/**
	 *
	 */
	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	private UserLogin getUserLoginFromToken() {
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private void validateProductTopping(final ProductVariant productVariant, final ProductToppingDto productToppingDto) throws ValidationException {
		validateDTOProperties(productToppingDto);
		Long vendorId = getVendorIdForLoginUser();
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(UNAUTHORIZED, null));
		}
		if (productToppingDto.getActive().booleanValue() && Boolean.FALSE.equals(productVariant.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
		}
		if (productToppingDto.getId() != null) {
			if (productToppingRepository
					.findByProductVariantAndNameAndVendorIdAndIdNot(productVariant, productToppingDto.getName(), vendorId, productToppingDto.getId())
					.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		} else {
			if (productToppingRepository.findByProductVariantAndVendorIdAndName(productVariant, vendorId, productToppingDto.getName()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		}
	}

	private void validateDTOProperties(final ProductToppingDto productToppingDto) throws ValidationException {
		if (productToppingDto.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.rate.not.null", null));
		} else if (productToppingDto.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productToppingDto.getName())) {
			throw new ValidationException(messageByLocaleService.getMessage("name.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productToppingDto.getDescription())) {
			throw new ValidationException(messageByLocaleService.getMessage("description.not.null", null));
		}
	}

}
