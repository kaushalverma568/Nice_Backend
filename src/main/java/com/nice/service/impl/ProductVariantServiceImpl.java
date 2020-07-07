package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.UserType;
import com.nice.dto.ProductAttributeValueDTO;
import com.nice.dto.ProductVariantRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductVariantMapper;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.UOM;
import com.nice.model.UserLogin;
import com.nice.repository.ProductVariantRepository;
import com.nice.service.ProductAddonsService;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.ProductService;
import com.nice.service.ProductToppingService;
import com.nice.service.ProductVariantService;
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

	// @Autowired
	// private DiscountService discountService;

	@Autowired
	private ProductVariantMapper productVariantMapper;

	@Autowired
	private UOMService uomService;
	//
	// @Autowired
	// private StoreService storeService;
	//
	// @Autowired
	// private StockDetailsService stockDetailsService;

	// @Autowired
	// private UsersService usersService;
	//
	// @Autowired
	// private CartItemService cartItemService;
	//
	// @Autowired
	// private TempCartItemService tempCartItemService;
	//
	// @Autowired
	// private PosCartService posCartService;
	@Autowired
	private ProductAddonsService productAddonsService;

	@Autowired
	private ProductToppingService productToppingService;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Override
	public void addUpdateProductVariantList(final Long productId, final List<ProductVariantRequestDTO> productVariantRequestDTOList)
			throws NotFoundException, ValidationException {
		final Product product = productService.getProductDetail(productId);
		for (ProductVariantRequestDTO productVariantRequestDTO : productVariantRequestDTOList) {
			validateProductVariant(product, productVariantRequestDTO);
			final ProductVariant productVariant = productVariantMapper.toEntity(productVariantRequestDTO);
			productVariant.setVendorId(product.getVendorId());
			if (productVariantRequestDTO.getId() == null) {
				productVariant.setUom(uomService.getUOMDetail(productVariantRequestDTO.getUomId()));
				productVariant.setProduct(product);
				// if (product.getDiscountId() != null) {
				// final Double discounteRate = discountService.getDiscountDetails(product.getDiscountId()).getDiscountRate();
				// productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
				// }
			} else {
				final ProductVariant existingProductVariant = getProductVariantDetail(productVariantRequestDTO.getId());
				if (!existingProductVariant.getProduct().getId().equals(productId)) {
					throw new ValidationException(messageByLocaleService.getMessage("product.id.not.unique", null));
				} else if (!existingProductVariant.getUom().getId().equals(productVariantRequestDTO.getUomId())) {
					throw new ValidationException(messageByLocaleService.getMessage("uom.id.not.unique", null));
				} else {
					productVariant.setUom(existingProductVariant.getUom());
					productVariant.setProduct(existingProductVariant.getProduct());
					// if (existingProductVariant.getProduct().getDiscountId() != null) {
					// final Double discounteRate =
					// discountService.getDiscountDetails(existingProductVariant.getProduct().getDiscountId()).getDiscountRate();
					// productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
					// }
				}
			}
			productVariantRepository.save(productVariant);
		}
	}

	/**
	 * @param productVariantRequestDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	private void validateProductVariant(final Product product, final ProductVariantRequestDTO productVariantRequestDTO)
			throws ValidationException, NotFoundException {
		validateDTOProperties(productVariantRequestDTO);
		final UOM uom = uomService.getUOMDetail(productVariantRequestDTO.getUomId());

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
			} else if (productVariantRepository.findBySkuIgnoreCaseAndIdNot(productVariantRequestDTO.getSku(), productVariantRequestDTO.getId()).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("sku.not.unique", null));
			}
		} else {
			if (productVariantRepository.findByProductAndUom(product, uom).isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("product.variant.not.unique", null));
			} else if (productVariantRepository.findBySkuIgnoreCase(productVariantRequestDTO.getSku()).isPresent()) {
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
	public List<ProductVariantResponseDTO> getProductVariantDetailByProduct(final Product product, final Boolean active)
			throws NotFoundException, ValidationException {
		List<ProductVariantResponseDTO> productVariantResponseDTOs = new ArrayList<>();
		List<ProductVariant> productVariants = getProductVariantByProduct(product, active);
		UserLogin userLogin = getUserLoginFromToken();
		Boolean isAdmin = false;
		if (userLogin.getEntityType() == null || UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			isAdmin = true;
		}
		for (ProductVariant productVariant : productVariants) {
			ProductVariantResponseDTO productVariantResponseDTO = convertToResponseDto(productVariant, isAdmin);
			productVariantResponseDTOs.add(productVariantResponseDTO);
		}
		return productVariantResponseDTOs;
	}

	// @Override
	// public List<ProductVariantResponseDTO> convertToResponseDtoList(final List<ProductVariant> productVariantList, final
	// Long pincodeId)
	// throws NotFoundException, ValidationException {
	// List<ProductVariantResponseDTO> productVariantResponseDTOList = new ArrayList<>();
	// for (ProductVariant productVariant : productVariantList) {
	// productVariantResponseDTOList.add(convertToResponseDto(productVariant));
	// }
	// return productVariantResponseDTOList;
	// }

	private ProductVariantResponseDTO convertToResponseDto(final ProductVariant productVariant, final boolean isAdmin) throws NotFoundException {
		ProductVariantResponseDTO productVariantResponseDTO = new ProductVariantResponseDTO();
		BeanUtils.copyProperties(productVariant, productVariantResponseDTO);
		productVariantResponseDTO.setId(productVariant.getId());
		productVariantResponseDTO.setProductId(productVariant.getProduct().getId());
		productVariantResponseDTO.setProductName(productVariant.getProduct().getName());
		productVariantResponseDTO.setUomId(productVariant.getUom().getId());
		productVariantResponseDTO.setUomMeasurement(productVariant.getUom().getMeasurement());
		productVariantResponseDTO.setUomQuantity(productVariant.getUom().getQuantity());
		productVariantResponseDTO.setUomLabel(productVariant.getUom().getUomLabel());
		// TODO
		/**
		 * Get available qty from inventory and set here, currently set as 0
		 */
		productVariantResponseDTO.setAvailableQty(0);
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
		productVariantResponseDTO.setProductAttributeValuesDtoMap(productAttributeValueDtoMap);
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
			if (Boolean.TRUE.equals(active)) {
				if (Boolean.FALSE.equals(productVariant.getUom().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("uom.activate.first", null));
				}
				if (Boolean.FALSE.equals(productVariant.getProduct().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.activate.first", null));
				}
			} else {
				// cartItemService.deleteCartItemsForProductVariant(productVariant.getId());
				// tempCartItemService.deleteCartItemsForProductVariant(productVariant.getId());
				// posCartService.deleteAllByProductVariant(productVariant.getId());
			}
			productVariant.setActive(active);
			productVariantRepository.save(productVariant);
		}
	}

	// private void changeStatusForDependentEntity(final Long productVariantId, final Boolean active) throws
	// NotFoundException, ValidationException {
	// List<ProductAddons> productAddonsList = productAddonsService.getList(true, productVariantId);
	// for (ProductAddons productAddons : productAddonsList) {
	// productAddonsService.changeStatus(productAddons.getId(), false);
	// }
	// }

	@Override
	public List<ProductVariantResponseDTO> getProductVariantProductList(final Long productId, final Boolean active, final Boolean isAdmin)
			throws NotFoundException, ValidationException {
		final Product product = productService.getProductDetail(productId);
		return getProductVariantDetailByProduct(product, active);
	}

	@Override
	public ProductVariantResponseDTO getProductVariant(final Long productVariantId) throws NotFoundException, ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		Boolean isAdmin = false;
		if (userLogin.getEntityType() == null || UserType.VENDOR.name().equals(userLogin.getEntityType())) {
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
		UserLogin userLogin = getUserLoginFromToken();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
		}
		/**
		 * Here is admin is taken as true as this method will only be accessed by admin/vendor always.
		 */
		return convertToResponseDto(getProductVariantDetailBySku(sku, userLogin.getEntityId()), true);
	}

	@Override
	public ProductVariant getProductVariantDetailBySku(final String sku, final Long vendorId) throws NotFoundException {
		return productVariantRepository.findBySkuIgnoreCaseAndVendorId(sku, vendorId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.variant.not.found", null)));
	}

	private UserLogin getUserLoginFromToken() {
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}
}
