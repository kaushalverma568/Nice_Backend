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
import com.nice.dto.ProductAddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAddonsMapper;
import com.nice.model.Addons;
import com.nice.model.CartAddons;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;
import com.nice.model.TempCartAddons;
import com.nice.model.UserLogin;
import com.nice.repository.ProductAddonsRepository;
import com.nice.service.AddonsService;
import com.nice.service.CartAddonsService;
import com.nice.service.ProductAddonsService;
import com.nice.service.ProductVariantService;
import com.nice.service.TempCartAddonsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service("productAddonsService")
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

	@Autowired
	private AddonsService addonsService;

	@Autowired
	private CartAddonsService cartAddonsService;

	@Autowired
	private TempCartAddonsService tempCartAddonsService;

	@Override
	public void addUpdateProductAddons(final List<ProductAddonsDTO> productAddonsDtoList, final Long productVariantId)
			throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		/**
		 * check of the vendor of product vendor is same as the one creating the addon
		 */
		if (!productVariant.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

		for (ProductAddonsDTO productAddonsDto : productAddonsDtoList) {
			Addons addons = addonsService.getAddonsById(productAddonsDto.getAddonsId());
			validateProductAddons(productVariant, productAddonsDto);
			if (isExists(productAddonsDto, productVariant, addons)) {
				throw new ValidationException(messageByLocaleService.getMessage("addons.already.exists", new Object[] { productAddonsDto.getAddonsName() }));
			}
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
			productAddons.setAddons(addons);
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
	}

	@Override
	public ProductAddonsDTO getProductAddons(final Long productAddonsId) throws NotFoundException {
		return productAddonsMapper.toDto(getProductAddonsDetail(productAddonsId));
	}

	@Override
	public void changeStatus(final Long productAddonsId, final Boolean active) throws ValidationException, NotFoundException {
		ProductAddons existingProductAddons = getProductAddonsDetail(productAddonsId);
		LOGGER.info("Existing  ProductAddons details {} ", existingProductAddons);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductAddons.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "product.attribute.active" : "product.attribute.deactive", null));
		} else {
			/**
			 * at time of active addon check product variant is active or not
			 */
			if (Boolean.TRUE.equals(active)) {
				if (Boolean.FALSE.equals(existingProductAddons.getProductVariant().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.variant.activate.first", null));
				}
			} else {
				/**
				 * get cartAddons and delete them in order to deactivate product addon
				 */
				List<CartAddons> cartAddonsList = cartAddonsService.getCartAddonsByProductAddOns(productAddonsId);
				for (CartAddons cartAddons : cartAddonsList) {
					cartAddonsService.deleteByCartAddonsId(cartAddons.getId());
				}
				List<TempCartAddons> tempCartAddonsList = tempCartAddonsService.getTempCartAddonsForProductAddons(productAddonsId);
				for (TempCartAddons tempCartAddons : tempCartAddonsList) {
					tempCartAddonsService.deleteByTempCartAddonsId(tempCartAddons.getId());
				}
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
			productAddonsList = productAddonsRepository.findAllByProductVariantAndActiveOrderByRateAsc(productVariant, activeRecords);
		} else {
			productAddonsList = productAddonsRepository.findAllByProductVariantOrderByRateAsc(productVariant);
		}
		return productAddonsList;
	}

	@Override
	public List<ProductAddonsDTO> getDtoListWithUserCheck(Boolean activeRecords, final Long productVariantId) throws NotFoundException, ValidationException {
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
		return getDtoList(activeRecords, productVariantId);
	}

	@Override
	public List<ProductAddonsDTO> getDtoList(final Boolean activeRecords, final Long productVariantId) throws NotFoundException {
		List<ProductAddons> productAddonsList = getList(activeRecords, productVariantId);
		return productAddonsMapper.toDtos(productAddonsList);
	}

	@Override
	public boolean isExists(final ProductAddonsDTO productAddonsDto, final ProductVariant productVariant, final Addons addons)
			throws ValidationException, NotFoundException {
		if (productAddonsDto.getId() != null) {
			return productAddonsRepository.findByProductVariantAndAddonsAndIdNot(productVariant, addons, productAddonsDto.getId()).isPresent();

		} else {
			return productAddonsRepository.findByProductVariantAndAddons(productVariant, addons).isPresent();
		}
	}

	@Override
	public ProductAddons getProductAddonsDetail(final Long productAddonsId) throws NotFoundException {
		return productAddonsRepository.findById(productAddonsId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND_ID, new Object[] { productAddonsId })));
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

	private void validateDTOProperties(final ProductAddonsDTO productAddonsDto) throws ValidationException {
		if (productAddonsDto.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.rate.not.null", null));
		} else if (productAddonsDto.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (productAddonsDto.getAddonsId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.not.null", null));
		}
	}

	@Override
	public List<ProductAddons> getListByAddonsId(final Long addonsId) throws NotFoundException {
		return productAddonsRepository.findAllByAddonsOrderByRateAsc(addonsService.getAddonsById(addonsId));
	}

}
