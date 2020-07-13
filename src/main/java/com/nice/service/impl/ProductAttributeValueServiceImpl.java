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
		LOGGER.info("Inside addUpdateProductAttributeValue");
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		/**
		 * check of the vendor of product vendor is same as the one creating the product attribute variant
		 */
		if (!productVariant.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		for (ProductAttributeValueDTO productAttributeValuesDto : productAttributeValueDTO) {
			ProductAttribute productAttribute = productAttributeService.getProductAttributeDetail(productAttributeValuesDto.getProductAttributeId());
			/**
			 * check if the vendor of product attribute is same as the one creating the product attribute variant
			 */
			if (!productAttribute.getVendorId().equals(vendorId)) {
				throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
			}

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
		LOGGER.info("After addUpdateProductAttributeValue");
	}

	@Override
	public ProductAttributeValueDTO getProductAttributeValue(final Long productAttributeValueId) throws NotFoundException {
		LOGGER.info("Inside getProductAttributeValue, productAttributeValueId : {}", productAttributeValueId);
		ProductAttributeValue productAttributeValue = getProductAttributeValueDetail(productAttributeValueId);
		LOGGER.info("After getProductAttributeValue, productAttributeValueId : {}", productAttributeValueId);
		return productAttributeValueMapper.toDto(productAttributeValue);
	}

	@Override
	public void changeStatus(final Long productAttributeValueId, final Boolean active) throws ValidationException, NotFoundException {
		ProductAttributeValue existingProductAttributeValue = getProductAttributeValueDetail(productAttributeValueId);
		LOGGER.info("Inside  ProductAttributeValue details {} ", existingProductAttributeValue);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductAttributeValue.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.value.deactive", null));
			}
		} else {
			if (Boolean.TRUE.equals(active)) {
				/**
				 * at time of active attribute value check product variant and attribute is active or not
				 */
				if (Boolean.FALSE.equals(existingProductAttributeValue.getProductVariant().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
				} else if (Boolean.FALSE.equals(existingProductAttributeValue.getProductAttribute().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.attribute.activate.first", null));
				}
			}
			existingProductAttributeValue.setActive(active);
			productAttributeValueRepository.save(existingProductAttributeValue);
		}
		LOGGER.info("Inside  productAttributeValueId details {} ", existingProductAttributeValue);
	}

	@Override
	public List<ProductAttributeValueDTO> getDtoListWithUserCheck(Boolean activeRecords, final Long productVariantId)
			throws NotFoundException, ValidationException {
		LOGGER.info("  ProductAttributeValue details, active : {} and productVariantId :{} ", activeRecords, productVariantId);
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * If the userLogin is null or userType is customer show only activeRecords irrespective of what is sent from front end.
		 */
		if (userLogin != null && (UserType.VENDOR.name().equals(userLogin.getEntityType()) && !productVariant.getVendorId().equals(userLogin.getEntityId()))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else if (userLogin == null || UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			activeRecords = true;
		}
		return getList(productVariantId, activeRecords);
	}

	@Override
	public List<ProductAttributeValueDTO> getList(final Long productVariantId, final Boolean activeRecords) throws NotFoundException {
		LOGGER.info("Inside ProductAttributeValue getList, active : {} and productVariantId :{} ", activeRecords, productVariantId);
		List<ProductAttributeValue> productAttributeValuesList = null;
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		if (activeRecords != null) {
			productAttributeValuesList = productAttributeValueRepository.findAllByProductVariantAndActive(productVariant, activeRecords);
		} else {
			productAttributeValuesList = productAttributeValueRepository.findAllByProductVariant(productVariant);
		}
		LOGGER.info("After ProductAttributeValue getList, active : {} and productVariantId :{} ", activeRecords, productVariantId);
		return productAttributeValueMapper.toDtos(productAttributeValuesList);
	}

	@Override
	public List<ProductAttributeValue> getListByProductAttributeOrActive(final Long productAttributeId, final Boolean activeRecords) throws NotFoundException {
		LOGGER.info("Inside getListByProductAttributeOrActive getList, active : {} and productAttributeId :{} ", activeRecords, productAttributeId);
		if (productAttributeId != null) {
			ProductAttribute productAttribute = productAttributeService.getProductAttributeDetail(productAttributeId);
			if (activeRecords != null) {
				return productAttributeValueRepository.findAllByProductAttributeAndActive(productAttribute, activeRecords);
			} else {
				return productAttributeValueRepository.findAllByProductAttribute(productAttribute);
			}
		} else {
			if (activeRecords != null) {
				return productAttributeValueRepository.findAllByActive(activeRecords);
			} else {
				return productAttributeValueRepository.findAll();
			}
		}
	}

	@Override
	public boolean isExists(final ProductAttributeValueDTO productAttributeValueDTO, final ProductVariant productVariant,
			final ProductAttribute productAttribute) throws NotFoundException {
		LOGGER.info("After ProductAttributeValue isExists");
		if (productAttributeValueDTO.getId() != null) {
			return productAttributeValueRepository.findByProductVariantAndProductAttributeAndAttributeValueAndIdNot(productVariant, productAttribute,
					productAttributeValueDTO.getAttributeValue(), productAttributeValueDTO.getId()).isPresent();
		} else {
			return productAttributeValueRepository
					.findByProductVariantAndProductAttributeAndAttributeValue(productVariant, productAttribute, productAttributeValueDTO.getAttributeValue())
					.isPresent();
		}
	}

	@Override
	public ProductAttributeValue getProductAttributeValueDetail(final Long productAttributeValueId) throws NotFoundException {
		LOGGER.info("After getProductAttributeValueDetail Details");
		return productAttributeValueRepository.findById(productAttributeValueId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productAttributeValueId })));
	}

	@Override
	public void deleteProductAttributeValue(final Long productAttributeValueId) {
		LOGGER.info("After deleteProductAttributeValue Details");
		productAttributeValueRepository.deleteById(productAttributeValueId);
	}

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
		if (productAddonsDto.getActive().booleanValue() && Boolean.FALSE.equals(productVariant.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
		}
	}
}
