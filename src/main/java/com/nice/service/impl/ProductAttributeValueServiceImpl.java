package com.nice.service.impl;

import java.util.List;

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
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAttributeValueMapper;
import com.nice.model.ProductAttribute;
import com.nice.model.ProductAttributeValue;
import com.nice.model.ProductVariant;
import com.nice.model.UserLogin;
import com.nice.repository.ProductAttributeValueRepository;
import com.nice.service.ProductAttributeService;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.ProductVariantService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ProductAttributeValueServiceImpl implements ProductAttributeValueService {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAttributeValueServiceImpl.class);

	private static final String NOT_FOUND = "product.attribute.value.not.found";

	@Autowired
	private ProductAttributeValueRepository productAttributeValueRepository;

	@Autowired
	private ProductAttributeValueMapper productAttributeValueMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductAttributeService productAttributeService;

	@Autowired
	private ProductVariantService productVariantService;

	@Override
	public void addUpdateProductAttributeValue(final List<ProductAttributeValueDTO> productAttributeValueDTO, final Long productVariantId)
			throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		for (ProductAttributeValueDTO productAttributeValuesDto : productAttributeValueDTO) {
			ProductAttribute productAttribute = productAttributeService.getProductAttributeDetail(productAttributeValuesDto.getProductAttributeId());
			if (isExists(productAttributeValuesDto, productVariant, productAttribute)) {
				throw new ValidationException(messageByLocaleService.getMessage("attribute.value.already.exists",
						new Object[] { productAttributeValuesDto.getAttributeValue(), productAttribute.getName() }));
			}
			validateProductAttributeValues(productVariant, productAttributeValuesDto);
			productAttributeValuesDto.setProductVariantId(productVariantId);
			ProductAttributeValue productAttributeValues = new ProductAttributeValue();
			if (productAttributeValuesDto.getId() != null) {
				productAttributeValues = getProductAttributeValueDetail(productAttributeValuesDto.getId());
				if (!productVariant.getVendorId().equals(vendorId)) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				} else if (!productAttributeValues.getProductVariant().getId().equals(productVariant.getId())) {
					throw new ValidationException(messageByLocaleService.getMessage("attribute.values.associated.to.variant", null));
				}
			}
			BeanUtils.copyProperties(productAttributeValuesDto, productAttributeValues);
			productAttributeValues.setProductAttribute(productAttribute);
			productAttributeValues.setProductVariant(productVariant);
			productAttributeValueRepository.save(productAttributeValues);
		}
	}

	@Override
	public ProductAttributeValueDTO getProductAttributeValue(final Long ProductAttributeValueId) throws NotFoundException {
		ProductAttributeValue productAttributeValue = productAttributeValueRepository.findById(ProductAttributeValueId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { ProductAttributeValueId })));
		return productAttributeValueMapper.toDto(productAttributeValue);
	}

	@Override
	public void changeStatus(final Long productAttributeValueId, final Boolean active) throws ValidationException, NotFoundException {
		ProductAttributeValue existingProductAttributeValue = productAttributeValueRepository.findById(productAttributeValueId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productAttributeValueId })));
		LOGGER.info("Existing  ProductAttributeValue details {} ", existingProductAttributeValue);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductAttributeValue.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.deactive", null));
			}
		} else {
			existingProductAttributeValue.setActive(active);
			productAttributeValueRepository.save(existingProductAttributeValue);
		}
	}

	@Override
	public List<ProductAttributeValueDTO> getList(final Long productVariantId, final Boolean activeRecords) throws NotFoundException {
		List<ProductAttributeValue> productAttributeValuesList = null;
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		if (activeRecords != null) {
			productAttributeValuesList = productAttributeValueRepository.findAllByProductVariantAndActive(productVariant, activeRecords);
		} else {
			productAttributeValuesList = productAttributeValueRepository.findAllByProductVariant(productVariant);
		}
		return productAttributeValueMapper.toDtos(productAttributeValuesList);
	}

	@Override
	public boolean isExists(final ProductAttributeValueDTO productAttributeValueDTO, final ProductVariant productVariant,
			final ProductAttribute productAttribute) throws NotFoundException {
		if (productAttributeValueDTO.getId() != null) {
			return !(productAttributeValueRepository.findByProductVariantAndProductAttributeAndAttributeValueAndIdNot(productVariant, productAttribute,
					productAttributeValueDTO.getAttributeValue(), productAttributeValueDTO.getId()).isEmpty());
		} else {
			return !(productAttributeValueRepository
					.findByProductVariantAndProductAttributeAndAttributeValue(productVariant, productAttribute, productAttributeValueDTO.getAttributeValue())
					.isEmpty());
		}
	}

	@Override
	public ProductAttributeValue getProductAttributeValueDetail(final Long ProductAttributeValueId) throws NotFoundException {
		return productAttributeValueRepository.findById(ProductAttributeValueId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { ProductAttributeValueId })));
	}

	@Override
	public void deleteProductAttributeValue(final Long productAttributeValueId) {
		productAttributeValueRepository.deleteById(productAttributeValueId);
	}

	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	private UserLogin getUserLoginFromToken() {
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private void validateDTOProperties(final ProductAttributeValueDTO productAttributeValuesDto) throws ValidationException {
		if (productAttributeValuesDto.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("attribute.value.rate.not.null", null));
		} else if (productAttributeValuesDto.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productAttributeValuesDto.getAttributeValue())) {
			throw new ValidationException(messageByLocaleService.getMessage("attribute.value.not.null", null));
		} else if (productAttributeValuesDto.getProductAttributeId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.attribute.not.null", null));
		}
	}

	/**
	 * @param productVariant
	 * @param productAddonsDto
	 * @throws ValidationException
	 */
	private void validateProductAttributeValues(final ProductVariant productVariant, final ProductAttributeValueDTO productAddonsDto)
			throws ValidationException {
		validateDTOProperties(productAddonsDto);

		Long vendorId = getVendorIdForLoginUser();
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productAddonsDto.getActive().booleanValue() && Boolean.FALSE.equals(productVariant.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
		}
		// if (productAddonsDto.getId() != null) {
		// if (productAttributeValueRepository
		// .findByProductVariantAndProductAttributeAndAttributeValueAndIdNot(productVariant, productAddonsDto.getName(),
		// vendorId, productAddonsDto.getId())
		// .isPresent()) {
		// throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
		// }
		// } else {
		// if (productAttributeValueRepository.findByProductVariantAndVendorIdAndName(productVariant, vendorId,
		// productAddonsDto.getName()).isPresent()) {
		// throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
		// }
		// }

	}
}
