package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

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
import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAddonsMapper;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;
import com.nice.model.UserLogin;
import com.nice.repository.ProductAddonsRepository;
import com.nice.service.ProductAddonsService;
import com.nice.service.ProductVariantService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ProductAddonsServiceImpl implements ProductAddonsService {

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAddonsServiceImpl.class);

	private static final String NOT_FOUND_ID = "addons.not.found";

	@Autowired
	private ProductAddonsRepository productAddonsRepository;

	@Autowired
	private ProductAddonsMapper productAddonsMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantService productVariantService;

	@Override
	public void addUpdateProductAddons(final List<ProductAddonsDTO> productAddonsDtoList, final Long productVariantId)
			throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		for (ProductAddonsDTO productAddonsDto : productAddonsDtoList) {
			if (isExists(productAddonsDto, productVariant)) {
				throw new ValidationException(messageByLocaleService.getMessage("addons.already.exists", new Object[] { productAddonsDto.getName() }));
			}
			validateProductAddons(productVariant, productAddonsDto);
			productAddonsDto.setProductVariantId(productVariantId);
			ProductAddons productAddons = new ProductAddons();
			if (productAddonsDto.getId() != null) {
				productAddons = getProductAddonsDetail(productAddonsDto.getId());
				if (!productAddons.getVendorId().equals(vendorId)) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				} else if (!productAddons.getProductVariant().getId().equals(productVariant.getId())) {
					throw new ValidationException(messageByLocaleService.getMessage("addons.associated.to.variant", null));
				}
			}
			BeanUtils.copyProperties(productAddonsDto, productAddons);
			productAddons.setVendorId(vendorId);
			productAddons.setProductVariant(productVariant);
			productAddonsRepository.save(productAddons);
		}
	}

	/**
	 * @param productVariant
	 * @param productAddonsDto
	 * @throws ValidationException
	 */
	private void validateProductAddons(final ProductVariant productVariant, final ProductAddonsDTO productAddonsDto) throws ValidationException {
		validateDTOProperties(productAddonsDto);
		Long vendorId = getVendorIdForLoginUser();
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productAddonsDto.getActive().booleanValue() && Boolean.FALSE.equals(productVariant.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
		}
		if (productAddonsDto.getId() != null) {
			if (productAddonsRepository
					.findByProductVariantAndNameAndVendorIdAndIdNot(productVariant, productAddonsDto.getName(), vendorId, productAddonsDto.getId())
					.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		} else {
			if (productAddonsRepository.findByProductVariantAndVendorIdAndName(productVariant, vendorId, productAddonsDto.getName()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
			}
		}

	}

	@Override
	public ProductAddonsDTO updateProductAddons(final ProductAddonsDTO productAddonsDto) throws NotFoundException, ValidationException {
		if (productAddonsDto.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.extras.id.not.null", null));
		}
		Optional<ProductAddons> optExistingProductAddons = productAddonsRepository.findById(productAddonsDto.getId());
		if (!optExistingProductAddons.isPresent()) {
			LOGGER.error("ProductAddons is not exists for ProductAddonsId {} ", productAddonsDto.getId());
			throw new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND_ID, new Object[] { productAddonsDto.getId() }));
		}
		return productAddonsMapper.toDto(productAddonsRepository.save(productAddonsMapper.toEntity(productAddonsDto)));
	}

	@Override
	public ProductAddonsDTO getProductAddons(final Long productAddonsId) throws NotFoundException {
		ProductAddons productAddons = productAddonsRepository.findById(productAddonsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND_ID, new Object[] { productAddonsId })));
		return productAddonsMapper.toDto(productAddons);
	}

	@Override
	public void changeStatus(final Long productAddonsId, final Boolean active) throws ValidationException, NotFoundException {
		ProductAddons existingProductAddons = productAddonsRepository.findById(productAddonsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND_ID, new Object[] { productAddonsId })));
		LOGGER.info("Existing  ProductAddons details {} ", existingProductAddons);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductAddons.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.deactive", null));
			}
		} else {
			/**
			 * at time of active addon check product variant is active or not
			 */
			if (Boolean.TRUE.equals(active) && Boolean.FALSE.equals(existingProductAddons.getProductVariant().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
			}
			existingProductAddons.setActive(active);
			productAddonsRepository.save(existingProductAddons);
		}
	}

	@Override
	public List<ProductAddons> getList(final Boolean activeRecords, final Long productVariantId) throws NotFoundException {
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		List<ProductAddons> productAddonsList = null;
		if (activeRecords != null) {
			productAddonsList = productAddonsRepository.findAllByProductVariantAndActive(productVariant, activeRecords);
		} else {
			productAddonsList = productAddonsRepository.findAllByProductVariant(productVariant);
		}
		return productAddonsList;
	}

	@Override
	public List<ProductAddonsDTO> getDtoList(final Boolean activeRecords, final Long productVariantId) throws NotFoundException {
		List<ProductAddons> productAddonsList = getList(activeRecords, productVariantId);
		return productAddonsMapper.toDtos(productAddonsList);
	}

	@Override
	public boolean isExists(final ProductAddonsDTO productAddonsDto, final ProductVariant productVariant) throws ValidationException {
		Long vendorId = getVendorIdForLoginUser();

		if (productAddonsDto.getId() != null) {
			return !(productAddonsRepository
					.findByProductVariantAndNameAndVendorIdAndIdNot(productVariant, productAddonsDto.getName(), vendorId, productAddonsDto.getId()).isEmpty());

		} else {
			return !(productAddonsRepository.findByProductVariantAndVendorIdAndName(productVariant, vendorId, productAddonsDto.getName()).isEmpty());
		}
	}

	@Override
	public ProductAddons getProductAddonsDetail(final Long ProductAddonsId) throws NotFoundException {
		return productAddonsRepository.findById(ProductAddonsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND_ID, new Object[] { ProductAddonsId })));
	}

	@Override
	public void deleteProductAddons(final Long productAddonsId) {
		productAddonsRepository.deleteById(productAddonsId);
	}

	/**
	 *
	 */
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

	private void validateDTOProperties(final ProductAddonsDTO productAddonsDto) throws ValidationException {
		if (productAddonsDto.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.rate.not.null", null));
		} else if (productAddonsDto.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productAddonsDto.getName())) {
			throw new ValidationException(messageByLocaleService.getMessage("name.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productAddonsDto.getDescription())) {
			throw new ValidationException(messageByLocaleService.getMessage("description.not.null", null));
		}
	}

}
