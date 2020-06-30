package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductMapper;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.repository.ProductRepository;
import com.nice.service.AssetService;
import com.nice.service.BrandService;
import com.nice.service.CategoryService;
import com.nice.service.FileStorageService;
import com.nice.service.ProductService;
import com.nice.service.ProductVariantService;
import com.nice.service.SubCategoryService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service(value = "productService")
@Transactional(rollbackFor = Throwable.class)
public class ProductServiceImpl implements ProductService {
	private static final String ID_NOT_NULL = "product.id.not.null";

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceImpl.class);

	@Autowired
	private ProductRepository productRepository;

	@Autowired
	private ProductMapper productMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private SubCategoryService subCategoryService;

	@Autowired
	private BrandService brandService;

	// @Autowired
	// private DiscountService discountService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private AssetService assetService;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public boolean isProductExists(final ProductRequestDTO productRequestDTO) {
		if (productRequestDTO.getId() != null) {
			return productRepository.findByNameIgnoreCaseAndBrandIdAndVendorIdAndIdNot(productRequestDTO.getName(), productRequestDTO.getBrandId(),
					productRequestDTO.getVendorId(), productRequestDTO.getId()).isPresent();
		} else {
			return productRepository
					.findByNameIgnoreCaseAndBrandIdAndVendorId(productRequestDTO.getName(), productRequestDTO.getBrandId(), productRequestDTO.getVendorId())
					.isPresent();
		}
	}

	@Override
	public void addProduct(final ProductRequestDTO productRequestDTO, final Long userId, final MultipartFile image)
			throws NotFoundException, ValidationException {

		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO, userId);
		product.setRating(0.0);
		product.setNoOfRating(0L);
		uploadImage(image, product);
		productRepository.save(product);
	}

	@Override
	public void updateProduct(final ProductRequestDTO productRequestDTO, final Long userId, final MultipartFile image)
			throws NotFoundException, ValidationException {
		if (productRequestDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage(ID_NOT_NULL, null));
		}
		final Product existingProduct = getProductDetail(productRequestDTO.getId());
		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO, userId);
		updateImages(image, existingProduct, product);
		productRepository.save(product);

	}

	private void updateImages(final MultipartFile image, final Product existingProduct, final Product product) {
		if (image != null) {
			deleteOldImage(existingProduct);
			uploadImage(image, product);
		} else {
			product.setImage(existingProduct.getImage());
			product.setImageOriginalName(existingProduct.getImageOriginalName());
		}
	}

	@Override
	public ProductResponseDTO getProduct(final Long productId, final Long customerId, final String uuid, final Boolean isAdmin, final Long pincodeId)
			throws NotFoundException, ValidationException {
		/**
		 * if is for customer then pin code id is mandatory and isAdmin=false is mandatory (for admin panel isAdmin will be
		 * null)
		 */
		if (isAdmin != null && (!isAdmin.booleanValue()) && pincodeId == null) {
			throw new ValidationException(messageByLocaleService.getMessage("pincode.id.not.null", null));
		}

		Product product = productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));

		return convertEntityToResponseDto(product, isAdmin, null);

	}

	@Override
	public Product getProductDetail(final Long productId) throws NotFoundException {
		return productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));
	}

	@Override
	public List<ProductResponseDTO> getProductListBasedOnParams(final ProductParamRequestDTO productParamRequestDTO, final Integer startIndex,
			final Integer pageSize, final Boolean listForAdmin, final Long pincodeId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get product list Based On Params {}", productParamRequestDTO);
		/**
		 * if is for customer then pin code id is mandatory
		 */
		if (listForAdmin != null && (!listForAdmin.booleanValue()) && pincodeId == null) {
			throw new ValidationException(messageByLocaleService.getMessage("pincode.id.not.null", null));
		}

		List<ProductResponseDTO> responseDTOs = new ArrayList<>();

		List<Product> products = productRepository.getProductListBasedOnParams(productParamRequestDTO, startIndex, pageSize);
		for (Product product : products) {
			final ProductResponseDTO responseDTO = convertEntityToResponseDto(product, listForAdmin, productParamRequestDTO);
			responseDTOs.add(responseDTO);
		}
		return responseDTOs;
	}

	@Override
	public Page<Product> getProductList(final Integer pageNumber, final Integer pageSize) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		return productRepository.findAll(pageable);
	}

	@Override
	public Page<Product> getProductListForVendor(final Integer pageNumber, final Integer pageSize, final Long vendorId) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		return productRepository.findAllByVendorId(pageable, vendorId);
	}

	/**
	 * validation for add or update product
	 *
	 * @param productRequestDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void validationForProduct(final ProductRequestDTO productRequestDTO) throws NotFoundException, ValidationException {
		/**
		 * for validation of made product foreign keys
		 */
		categoryService.getCategoryDetail(productRequestDTO.getCategoryId());
		subCategoryService.getSubCategoryDetail(productRequestDTO.getSubcategoryId());
		brandService.getBrandDetail(productRequestDTO.getBrandId());

		// TODO
		/**
		 *
		 * Validation for Vendor and Cuisine to be added once master are available
		 */

	}

	@Override
	public List<ProductResponseDTO> getProductDetailList(final List<Product> products) throws NotFoundException, ValidationException {
		List<ProductResponseDTO> productResponseDTOs = new ArrayList<>();
		for (Product product : products) {
			productResponseDTOs.add(convertEntityToResponseDto(product, false, null));
		}
		return productResponseDTOs;
	}

	/**
	 * listForAdmin==null means get product detail for admin listForAdmin==true means get product list for admin
	 *
	 * convert entity to response dto
	 *
	 * @param product
	 * @param listForAdmin
	 * @param productParamRequestDTO
	 * @param pincodeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	private ProductResponseDTO convertEntityToResponseDto(final Product product, final Boolean listForAdmin,
			final ProductParamRequestDTO productParamRequestDTO) throws NotFoundException, ValidationException {
		/**
		 * set foreign key values to response DTO
		 */
		ProductResponseDTO productResponseDTO = productMapper.toResponseDto(product);
		productResponseDTO.setCategoryName(categoryService.getCategoryDetail(productResponseDTO.getCategoryId()).getName());
		productResponseDTO.setSubcategoryName(subCategoryService.getSubCategoryDetail(productResponseDTO.getSubcategoryId()).getName());
		productResponseDTO.setBrandName(brandService.getBrandDetail(productResponseDTO.getBrandId()).getName());
		productResponseDTO.setImage(CommonUtility.getGeneratedUrl(product.getImage(), AssetConstant.PRODUCT_DIR));
		/**
		 * if we are fetching product list For admin then set product variants to empty list
		 */
		List<ProductVariantResponseDTO> productVariantList = new ArrayList<>();
		//
		// if (listForAdmin != null && listForAdmin.booleanValue()) {
		// productResponseDTO.setProductVariantList(Collections.emptyList());
		// } else if (productParamRequestDTO != null &&
		// CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productParamRequestDTO.getPriceRangeDTOList())) {
		// productVariantList = productVariantService.convertToResponseDtoList(
		// productRepository.getProductVariantListAccordingToPriceFilter(productParamRequestDTO, product.getId()), pincodeId);
		// } else {
		// productVariantList = productVariantService.getProductVariantDetailByProduct(product, null, pincodeId, null,
		// listForAdmin == null);
		// }

		/**
		 * if product variant is null/empty or availableQty=0 then product will go out of stock
		 *
		 */
		Integer availableQty = 0;
		for (ProductVariantResponseDTO productVariantResponseDTO : productVariantList) {
			availableQty += productVariantResponseDTO.getAvailableQty();
		}
		productResponseDTO.setProductOutOfStock(availableQty <= 0);

		productResponseDTO.setProductVariantList(CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productVariantList) ? productVariantList : Collections.emptyList());

		// if (product.getDiscountId() != null) {
		// productResponseDTO.setDiscountStatus(discountService.getDiscountDetails(product.getDiscountId()).getStatus());
		// }
		return productResponseDTO;
	}

	@Override
	public void changeStatus(final Long productId, final Boolean active, final Long userId) throws NotFoundException, ValidationException {
		Product product = getProductDetail(productId);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (product.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage("product.active.deactive", new Object[] { (Boolean.TRUE.equals(active) ? "active" : "deActive") }));
		} else {
			changeStatusForDependentEntity(active, userId, product);
			product.setActive(active);
			product.setUpdatedBy(userId);
			productRepository.save(product);
		}
	}

	/**
	 * @param productId
	 * @param active
	 * @param userId
	 * @param existingProduct
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public void changeStatusForDependentEntity(final Boolean active, final Long userId, final Product existingProduct)
			throws NotFoundException, ValidationException {
		if (Boolean.FALSE.equals(active)) {
			/**
			 * if active is false then deactivate its variants.
			 */
			List<ProductVariant> existingVariants = productVariantService.getProductVariantByProduct(existingProduct, true);
			for (ProductVariant productVariant : existingVariants) {
				productVariantService.changeStatus(productVariant.getId(), false, userId);
			}

		} else {
			validationForActivateProduct(existingProduct);
		}
	}

	/**
	 * check masters for activate product
	 *
	 * @param existingProduct
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void validationForActivateProduct(final Product existingProduct) throws NotFoundException, ValidationException {
		if (Boolean.FALSE.equals(categoryService.getCategoryDetail(existingProduct.getCategoryId()).getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("category.activate.first", null));
		}
		if (Boolean.FALSE.equals(subCategoryService.getSubCategoryDetail(existingProduct.getSubcategoryId()).getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("subcategory.activate.first", null));
		}
		if (Boolean.FALSE.equals(brandService.getBrandDetail(existingProduct.getBrandId()).getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("brand.activate.first", null));
		}
	}

	@Override
	public List<Product> getProductListBasedOnParamsWithoutPagination(final ProductParamRequestDTO productParamRequestDTO) {
		return productRepository.getProductListBasedOnParams(productParamRequestDTO, null, null);
	}

	@Override
	public Long getProductCountBasedOnParams(final ProductParamRequestDTO productParamRequestDTO) {
		return productRepository.getProductCountBasedOnParams(productParamRequestDTO);
	}

	/**
	 * upload image of product
	 *
	 * @param image
	 * @param product
	 */
	private void uploadImage(final MultipartFile image, final Product product) {
		product.setImage(assetService.saveAsset(image, AssetConstant.PRODUCT_DIR, 0));
		product.setImageOriginalName(image.getOriginalFilename());
	}

	/**
	 * delete old image
	 *
	 * @param product
	 */
	private void deleteOldImage(final Product product) {
		fileStorageService.deleteFile(product.getImage(), AssetConstant.PRODUCT_DIR);
	}

	// @Override
	// public GlobalSearchResponseDTO getResultOfGlobalSearch(final String searchKeyword) {
	// Map<Long, String> productMap;
	// Map<Long, String> categoryMap;
	// Map<Long, String> subCategoryMap;
	// Map<Long, String> brandMap;
	//
	// /**
	// * get all product,category,sub category and brand which is active and their name contains search keyword
	// */
	// ProductParamRequestDTO productParamRequestDTO = new ProductParamRequestDTO();
	// productParamRequestDTO.setActiveRecords(true);
	// productParamRequestDTO.setProductVariantActiveRecords(true);
	// productParamRequestDTO.setSearchKeyword(searchKeyword);
	// List<Product> productList = productRepository.getProductListBasedOnParams(productParamRequestDTO, null, null);
	// List<Category> categoryList = categoryService.getCategoryList(true, searchKeyword);
	// List<SubCategory> subCategoryList = subCategoryService.getSubCategoryList(true, searchKeyword);
	// List<Brand> brandList = brandService.getBrandList(true, searchKeyword);
	// productMap = productList.stream().collect(Collectors.toMap(Product::getId, Product::getName));
	// categoryMap = categoryList.stream().collect(Collectors.toMap(Category::getId, Category::getName));
	// subCategoryMap = subCategoryList.stream().collect(Collectors.toMap(SubCategory::getId, SubCategory::getName));
	// brandMap = brandList.stream().collect(Collectors.toMap(Brand::getId, Brand::getName));
	//
	// GlobalSearchResponseDTO globalSearchResponseDTO = new GlobalSearchResponseDTO();
	// globalSearchResponseDTO.setProductMap(productMap);
	// globalSearchResponseDTO.setCategoryMap(categoryMap);
	// globalSearchResponseDTO.setSubCategoryMap(subCategoryMap);
	// globalSearchResponseDTO.setBrandMap(brandMap);
	// return globalSearchResponseDTO;
	// }

	// @Override
	// public List<CategoryWiseProductCountDTO> getCategoryWiseProductCountList() {
	// return productRepository.getCategoryWiseProductCountList();
	// }

	// @Override
	// public void changeStatusOfIsFeaturedProduct(final Long productId, final Boolean active, final Long userId) throws
	// ValidationException, NotFoundException {
	// Product product = getProductDetail(productId);
	// if (active == null) {
	// throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
	// } else {
	// product.setUpdatedBy(userId);
	// productRepository.save(product);
	// }
	// }

}
