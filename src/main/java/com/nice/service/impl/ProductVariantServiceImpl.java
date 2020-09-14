package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ProductAttributeResponseDTO;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.ProductToppingResponseDTO;
import com.nice.dto.ProductVariantRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductVariantMapper;
import com.nice.model.BusinessCategory;
import com.nice.model.Product;
import com.nice.model.ProductAddons;
import com.nice.model.ProductVariant;
import com.nice.model.UOM;
import com.nice.model.UserLogin;
import com.nice.repository.ProductVariantRepository;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CartItemService;
import com.nice.service.DiscountService;
import com.nice.service.ProductAddonsService;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.ProductService;
import com.nice.service.ProductToppingService;
import com.nice.service.ProductVariantService;
import com.nice.service.StockDetailsService;
import com.nice.service.TempCartItemService;
import com.nice.service.UOMService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service("niceProductVariantService")
@Transactional(rollbackFor = Throwable.class)
public class ProductVariantServiceImpl implements ProductVariantService {

	/**
	 *
	 */

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductVariantRepository productVariantRepository;

	@Autowired
	private ProductService productService;

	@Autowired
	private ProductVariantMapper productVariantMapper;

	@Autowired
	private UOMService uomService;

	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private ProductAddonsService productAddonsService;

	@Autowired
	private ProductToppingService productToppingService;

	@Autowired
	private DiscountService discountService;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private StockDetailsService stockDetailsService;

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Override
	public void addUpdateProductVariantList(final Long productId, final ProductVariantRequestDTO productVariantRequestDTO)
			throws NotFoundException, ValidationException {
		final Product product = productService.getProductDetail(productId);
		Long vendorId = getVendorIdForLoginUser();
		if (!product.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		validateProductVariant(product, productVariantRequestDTO);
		final ProductVariant productVariant = productVariantMapper.toEntity(productVariantRequestDTO);
		productVariant.setVendorId(product.getVendorId());
		if (productVariantRequestDTO.getId() == null) {
			productVariant.setUom(uomService.getUOMDetail(productVariantRequestDTO.getUomId()));
			productVariant.setProduct(product);
			if (product.getDiscountId() != null) {
				final Double discounteRate = discountService.getDiscountDetails(product.getDiscountId()).getDiscountRate();
				productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
			}
		} else {
			final ProductVariant existingProductVariant = getProductVariantDetail(productVariantRequestDTO.getId());
			if (!existingProductVariant.getProduct().getId().equals(productId)) {
				throw new ValidationException(messageByLocaleService.getMessage("product.id.not.unique", null));
			} else if (!existingProductVariant.getUom().getId().equals(productVariantRequestDTO.getUomId())) {
				throw new ValidationException(messageByLocaleService.getMessage("cannot.change.uom", null));
			} else {
				productVariant.setUom(existingProductVariant.getUom());
				productVariant.setProduct(existingProductVariant.getProduct());
				if (existingProductVariant.getProduct().getDiscountId() != null) {
					final Double discounteRate = discountService.getDiscountDetails(existingProductVariant.getProduct().getDiscountId()).getDiscountRate();
					productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
				}
			}
		}
		productVariantRepository.save(productVariant);
	}

	/**
	 * @param productVariantRequestDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void validateProductVariant(final Product product, final ProductVariantRequestDTO productVariantRequestDTO)
			throws ValidationException, NotFoundException {
		validateDTOProperties(productVariantRequestDTO);
		Long vendorId = getVendorIdForLoginUser();
		final UOM uom = uomService.getUOMDetail(productVariantRequestDTO.getUomId());
		if (!uom.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productVariantRequestDTO.getActive()) {
			if (Boolean.FALSE.equals(uom.getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("uom.activate.first", null));
			}
			if (Boolean.FALSE.equals(product.getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("product.activate.first", null));
			}
		}
		if (productVariantRequestDTO.getId() != null) {

			if (productVariantRepository.findByProductAndUomAndIdNot(product, uom, productVariantRequestDTO.getId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.not.unique", null));
			} else if (productVariantRepository
					.findBySkuIgnoreCaseAndVendorIdAndIdNot(productVariantRequestDTO.getSku(), product.getVendorId(), productVariantRequestDTO.getId())
					.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("sku.not.unique", null));
			}
		} else {
			if (productVariantRepository.findByProductAndUom(product, uom).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.not.unique", null));
			} else if (productVariantRepository.findBySkuIgnoreCaseAndVendorId(productVariantRequestDTO.getSku(), product.getVendorId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("sku.not.unique", null));
			}
		}
	}

	private void validateDTOProperties(final ProductVariantRequestDTO productVariantRequestDTO) throws ValidationException {
		if (productVariantRequestDTO.getUomId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("uom.id.not.null", null));
		} else if (productVariantRequestDTO.getRate() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("rate.not.null", null));
		} else if (productVariantRequestDTO.getActive() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productVariantRequestDTO.getSku())) {
			throw new ValidationException(messageByLocaleService.getMessage("sku.not.null", null));
		}
	}

	@Override
	public List<ProductVariant> getProductVariantByProduct(final Product product, final Boolean active) {
		if (active != null) {
			return productVariantRepository.findAllByProductAndActive(product, active);
		} else {
			return productVariantRepository.findAllByProduct(product);
		}
	}

	@Override
	public List<ProductVariantResponseDTO> getProductVariantDetailByProduct(final Product product, final Boolean active, final Boolean isAdmin)
			throws NotFoundException, ValidationException {
		List<ProductVariantResponseDTO> productVariantResponseDTOs = new ArrayList<>();
		List<ProductVariant> productVariants = getProductVariantByProduct(product, active);

		for (ProductVariant productVariant : productVariants) {
			ProductVariantResponseDTO productVariantResponseDTO = convertToResponseDto(productVariant, isAdmin);
			productVariantResponseDTOs.add(productVariantResponseDTO);
		}
		return productVariantResponseDTOs;
	}

	private ProductVariantResponseDTO convertToResponseDto(final ProductVariant productVariant, final boolean isAdmin) throws NotFoundException {
		ProductVariantResponseDTO productVariantResponseDTO = new ProductVariantResponseDTO();
		BeanUtils.copyProperties(productVariant, productVariantResponseDTO);
		productVariantResponseDTO.setId(productVariant.getId());
		productVariantResponseDTO.setProductId(productVariant.getProduct().getId());
		productVariantResponseDTO.setProductNameArabic(productVariant.getProduct().getNameArabic());
		productVariantResponseDTO.setProductNameEnglish(productVariant.getProduct().getNameEnglish());
		productVariantResponseDTO.setUomMeasurementEnglish(productVariant.getUom().getMeasurementEnglish());
		productVariantResponseDTO.setUomMeasurementArabic(productVariant.getUom().getMeasurementArabic());
		productVariantResponseDTO.setUomLabelEnglish(productVariant.getUom().getUomLabelEnglish());
		productVariantResponseDTO.setUomLabelArabic(productVariant.getUom().getUomLabelArabic());
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			productVariantResponseDTO.setProductName(productVariantResponseDTO.getProductNameEnglish());
			productVariantResponseDTO.setUomMeasurement(productVariantResponseDTO.getUomMeasurementEnglish());
			productVariantResponseDTO.setUomLabel(productVariantResponseDTO.getUomLabelEnglish());
		} else {
			productVariantResponseDTO.setProductName(productVariantResponseDTO.getProductNameArabic());
			productVariantResponseDTO.setUomMeasurement(productVariantResponseDTO.getUomMeasurementArabic());
			productVariantResponseDTO.setUomLabel(productVariantResponseDTO.getUomLabelArabic());
		}
		productVariantResponseDTO.setUomId(productVariant.getUom().getId());

		productVariantResponseDTO.setUomQuantity(productVariant.getUom().getQuantity());

		BusinessCategory businessCategory = businessCategoryService.getBusinessCategoryDetail(productVariant.getProduct().getId());

		if (!businessCategory.getNameEnglish().equalsIgnoreCase(Constant.BUSINESS_CATEGORY_FOOD_DELIVERY)) {
			productVariantResponseDTO.setAvailableQty(stockDetailsService.getCountForVariant(productVariant).intValue());
		}
		/**
		 * Set product addons, attribute values and toppings list for the product variant
		 */
		productVariantResponseDTO.setProductAddonsDtoList(productAddonsService.getDtoList(isAdmin ? null : Boolean.TRUE, productVariant.getId()));
		List<ProductAttributeValueDTO> productAttributeValueDtoList = productAttributeValueService.getList(productVariant.getId(),
				isAdmin ? null : Boolean.TRUE);
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

		/**
		 * Convert Map to List
		 *
		 **/
		List<ProductAttributeResponseDTO> productAttributeResponseDtoList = new ArrayList<>();
		for (Map.Entry<String, List<ProductAttributeValueDTO>> productAttributeValueDto : productAttributeValueDtoMap.entrySet()) {
			List<ProductAttributeValueDTO> productAttributeValDtoList = productAttributeValueDto.getValue();
			ProductAttributeResponseDTO productAttributeResponseDto = new ProductAttributeResponseDTO();
			productAttributeResponseDto.setProductAttributeValueList(productAttributeValDtoList);
			productAttributeResponseDto.setAttributeName(productAttributeValueDto.getKey());
			productAttributeResponseDtoList.add(productAttributeResponseDto);
		}

		productVariantResponseDTO.setProductAttributeValuesDtoList(productAttributeResponseDtoList);

		productVariantResponseDTO
				.setProductToppingsDtoList(productToppingService.getToppingForProductVariant(productVariant.getId(), isAdmin ? null : Boolean.TRUE));
		return productVariantResponseDTO;
	}

	@Override
	public ProductVariant getProductVariantDetail(final Long variantId) throws NotFoundException {
		return productVariantRepository.findById(variantId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.variant.not.found", new Object[] { variantId })));
	}

	@Override
	public Optional<ProductVariant> getProductVariantDetailByProductAndUOMOptional(final Product product, final UOM uom) {
		return productVariantRepository.findByProductAndUom(product, uom);
	}

	@Override
	public ProductVariant getProductVariantDetailByProductAndUOM(final Product product, final UOM uom) throws NotFoundException {
		return getProductVariantDetailByProductAndUOMOptional(product, uom).orElseThrow(() -> new NotFoundException(
				messageByLocaleService.getMessage("product.variant.uom.doesnot.exist", new Object[] { product.getId(), uom.getId() })));
	}

	@Override
	public void changeStatus(final Long productVariantId, final Boolean active) throws NotFoundException, ValidationException {
		ProductVariant productVariant = getProductVariantDetail(productVariantId);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (productVariant.getActive().equals(active)) {
			if (Boolean.TRUE.equals(active)) {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.already.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.already.deactive", null));
			}

		} else {
			changeStatusOfDependentEntity(active, productVariant);
			productVariant.setActive(active);
			productVariantRepository.save(productVariant);
		}
	}

	/**
	 * @param productVariantId
	 * @param active
	 * @param productVariant
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void changeStatusOfDependentEntity(final Boolean active, final ProductVariant productVariant) throws ValidationException, NotFoundException {
		if (Boolean.TRUE.equals(active)) {
			if (Boolean.FALSE.equals(productVariant.getUom().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("uom.activate.first", null));
			}
			if (Boolean.FALSE.equals(productVariant.getProduct().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("product.activate.first", null));
			}
		} else {
			/**
			 * deActive addons which is active for this variant
			 */
			List<ProductAddons> productAddonsList = productAddonsService.getList(true, productVariant.getId());
			for (ProductAddons productAddons : productAddonsList) {
				productAddonsService.changeStatus(productAddons.getId(), false);
			}
			/**
			 * deActive toppings which is active for this variant
			 */
			List<ProductToppingResponseDTO> productToppingList = productToppingService.getToppingForProductVariant(productVariant.getId(), true);
			for (ProductToppingResponseDTO productToppingDto : productToppingList) {
				productToppingService.changeStatus(productToppingDto.getId(), false);
			}
			/**
			 * deActive all active product attribute values for this variant
			 */
			List<ProductAttributeValueDTO> productAttributeValueList = productAttributeValueService.getList(productVariant.getId(), true);
			for (ProductAttributeValueDTO productAttributeValue : productAttributeValueList) {
				productAttributeValueService.changeStatus(productAttributeValue.getId(), false);
			}
			/**
			 * remove this variant from all customer's cart
			 */
			cartItemService.deleteCartItemsForProductVariant(productVariant.getId());
			tempCartItemService.deleteCartItemsForProductVariant(productVariant.getId());
		}
	}

	@Override
	public List<ProductVariantResponseDTO> getProductVariantProductList(final Long productId, final Boolean active)
			throws NotFoundException, ValidationException {
		final Product product = productService.getProductDetail(productId);
		UserLogin userLogin = getUserLoginFromToken();
		Boolean isAdmin = false;
		/**
		 * if the user is a Vendor then check if the product belongs to him. For Super Admin all products should be visible
		 */
		if (userLogin != null && (userLogin.getEntityType() == null || UserType.VENDOR.name().equals(userLogin.getEntityType()))) {
			isAdmin = true;
			if (userLogin.getEntityId() != null && !product.getVendorId().equals(userLogin.getEntityId())) {
				throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
			}
		}
		return getProductVariantDetailByProduct(product, active, isAdmin);
	}

	@Override
	public ProductVariantResponseDTO getProductVariant(final Long productVariantId) throws NotFoundException, ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		Boolean isAdmin = false;
		if (userLogin != null && (userLogin.getEntityType() == null || UserType.VENDOR.name().equals(userLogin.getEntityType()))) {
			isAdmin = true;
		}
		return getProductVariantInternal(productVariantId, isAdmin);
	}

	/**
	 * This method will be used only for internal calls to skip the authentication process
	 *
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Override
	public ProductVariantResponseDTO getProductVariantInternal(final Long productVariantId, final Boolean isAdmin)
			throws NotFoundException, ValidationException {
		return convertToResponseDto(getProductVariantDetail(productVariantId), isAdmin);
	}

	@Override
	public ProductVariantResponseDTO getProductVariantBySku(final String sku) throws NotFoundException, ValidationException {
		UserLogin userLogin = checkForUserLogin();
		/**
		 * Here is admin is taken as true as this method will only be accessed by admin/vendor always.
		 */
		ProductVariant productVariant = getProductVariantDetailBySku(sku, userLogin.getEntityId());
		if (userLogin.getEntityId() == null || productVariant.getVendorId().equals(userLogin.getEntityId())) {
			return convertToResponseDto(getProductVariantDetailBySku(sku, userLogin.getEntityId()), true);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
	}

	@Override
	public ProductVariant getProductVariantDetailBySku(final String sku, final Long vendorId) throws NotFoundException {
		return productVariantRepository.findBySkuIgnoreCaseAndVendorId(sku, vendorId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.variant.not.found", null)));
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
	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}
}
