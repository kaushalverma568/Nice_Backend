package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.CategoryResponseDTO;
import com.nice.dto.CategoryWiseProductCountDTO;
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
import com.nice.model.UserLogin;
import com.nice.repository.ProductRepository;
import com.nice.service.AssetService;
import com.nice.service.BrandService;
import com.nice.service.CategoryService;
import com.nice.service.CuisineService;
import com.nice.service.FileStorageService;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductService;
import com.nice.service.ProductVariantService;
import com.nice.service.SubCategoryService;
import com.nice.service.VendorCuisineService;
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

	@Autowired
	private CuisineService cuisineService;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Autowired
	private VendorCuisineService vendorCuisineService;

	private UserLogin getUserLoginFromToken() {
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	@Override
	public boolean isProductExists(final ProductRequestDTO productRequestDTO) {
		UserLogin userLogin = getUserLoginFromToken();
		productRequestDTO.setVendorId(userLogin.getEntityId());
		if (productRequestDTO.getId() != null) {
			if (productRequestDTO.getBrandId() != null) {
				return productRepository.findByNameIgnoreCaseAndBrandIdAndVendorIdAndIdNot(productRequestDTO.getName(), productRequestDTO.getBrandId(),
						productRequestDTO.getVendorId(), productRequestDTO.getId()).isPresent();
			} else {
				return productRepository.findByNameIgnoreCaseAndCuisineIdAndVendorIdAndIdNot(productRequestDTO.getName(), productRequestDTO.getCuisineId(),
						productRequestDTO.getVendorId(), productRequestDTO.getId()).isPresent();
			}

		} else {
			if (productRequestDTO.getBrandId() != null) {
				return productRepository
						.findByNameIgnoreCaseAndBrandIdAndVendorId(productRequestDTO.getName(), productRequestDTO.getBrandId(), productRequestDTO.getVendorId())
						.isPresent();
			} else {
				return productRepository.findByNameIgnoreCaseAndCuisineIdAndVendorId(productRequestDTO.getName(), productRequestDTO.getBrandId(),
						productRequestDTO.getVendorId()).isPresent();
			}

		}
	}

	@Override
	public void addProduct(final ProductRequestDTO productRequestDTO, final MultipartFile image) throws NotFoundException, ValidationException {

		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO);
		/**
		 * Set some default values when product is created
		 */
		product.setRating(0.0);
		product.setNoOfRating(0L);
		uploadImage(image, product);
		productRepository.save(product);
	}

	@Override
	public void updateProduct(final ProductRequestDTO productRequestDTO, final MultipartFile image) throws NotFoundException, ValidationException {
		if (productRequestDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage(ID_NOT_NULL, null));
		}
		final Product existingProduct = getProductDetail(productRequestDTO.getId());
		productRequestDTO.setVendorId(existingProduct.getVendorId());
		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO);
		product.setRating(existingProduct.getRating());
		product.setNoOfRating(existingProduct.getNoOfRating());
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
	public ProductResponseDTO getProduct(final Long productId, final String uuid) throws NotFoundException, ValidationException {
		Boolean isAdmin = true;
		UserLogin userLogin = getUserLoginFromToken();
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			isAdmin = false;
		}
		Product product = productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));
		return convertEntityToResponseDto(product, isAdmin);

	}

	@Override
	public Product getProductDetail(final Long productId) throws NotFoundException {
		return productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));
	}

	@Override
	public List<ProductResponseDTO> getProductListBasedOnParams(final ProductParamRequestDTO productParamRequestDTO, final Integer startIndex,
			final Integer pageSize) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get product list Based On Params {}", productParamRequestDTO);
		UserLogin userLogin = getUserLoginFromToken();
		Boolean listForAdmin = true;
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setVendorId(userLogin.getEntityId());
		} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setActiveRecords(true);
			listForAdmin = false;
		}
		List<ProductResponseDTO> responseDTOs = new ArrayList<>();

		List<Product> products = productRepository.getProductListBasedOnParams(productParamRequestDTO, startIndex, pageSize);
		for (Product product : products) {
			final ProductResponseDTO responseDTO = convertEntityToResponseDto(product, listForAdmin);
			responseDTOs.add(responseDTO);
		}
		return responseDTOs;
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
		 * If the login user is not a vendor then throw an exception
		 */
		UserLogin userLogin = getUserLoginFromToken();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productRequestDTO.getId() != null && !productRequestDTO.getVendorId().equals(userLogin.getEntityId())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

		/**
		 * for validation of made product foreign keys
		 */
		categoryService.getCategoryDetail(productRequestDTO.getCategoryId());
		subCategoryService.getSubCategoryDetail(productRequestDTO.getSubcategoryId());
		brandService.getBrandDetail(productRequestDTO.getBrandId());
		cuisineService.getCuisineDetails(productRequestDTO.getCuisineId());
		if (productRequestDTO.getCuisineId() != null) {
			vendorCuisineService.getVendorCuisineByVendorIdAndCuisineId(productRequestDTO.getVendorId(), productRequestDTO.getCuisineId());
		}
	}

	@Override
	public List<ProductResponseDTO> getProductDetailList(final List<Product> products) throws NotFoundException, ValidationException {
		List<ProductResponseDTO> productResponseDTOs = new ArrayList<>();
		for (Product product : products) {
			productResponseDTOs.add(convertEntityToResponseDto(product, false));
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

	private ProductResponseDTO convertEntityToResponseDto(final Product product, final Boolean listForAdmin) throws NotFoundException, ValidationException {
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

		if (listForAdmin != null && listForAdmin.booleanValue()) {
			productResponseDTO.setProductVariantList(Collections.emptyList());
		} else {
			productVariantList = productVariantService.getProductVariantDetailByProduct(product, null);
		}

		/**
		 * if product variant is null/empty or availableQty=0 then product will go out of stock
		 *
		 */
		// TODO
		/**
		 * Grocery needs to be a hardcoded category here and its Id should be replaced here, as we dont need the available qty
		 * and productOutOfStock for any other category type except grocery.
		 */
		if (false) {
			Integer availableQty = 0;
			for (ProductVariantResponseDTO productVariantResponseDTO : productVariantList) {
				availableQty += productVariantResponseDTO.getAvailableQty();
			}
			productResponseDTO.setProductOutOfStock(availableQty <= 0);
		}

		productResponseDTO.setProductVariantList(CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productVariantList) ? productVariantList : Collections.emptyList());
		productResponseDTO.setProductExtrasList(productExtrasService.getList(product.getId(), listForAdmin ? null : true));
		// if (product.getDiscountId() != null) {
		// productResponseDTO.setDiscountStatus(discountService.getDiscountDetails(product.getDiscountId()).getStatus());
		// }
		return productResponseDTO;
	}

	@Override
	public void changeStatus(final Long productId, final Boolean active) throws NotFoundException, ValidationException {
		Product product = getProductDetail(productId);
		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * Only the vendor who created the produce and the admin can deactivate the product
		 */
		if (!((UserType.VENDOR.name().equals(userLogin.getEntityType()) && product.getVendorId().equals(userLogin.getEntityId()))
				|| userLogin.getEntityType() == null)) {
			throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
		}
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (product.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage("product.active.deactive", new Object[] { (Boolean.TRUE.equals(active) ? "active" : "deActive") }));
		} else {
			changeStatusForDependentEntity(active, product);
			product.setActive(active);
			productRepository.save(product);
		}
	}

	/**
	 * @param active
	 * @param existingProduct
	 * @param productId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public void changeStatusForDependentEntity(final Boolean active, final Product existingProduct) throws NotFoundException, ValidationException {
		if (Boolean.FALSE.equals(active)) {
			/**
			 * if active is false then deactivate its variants.
			 */
			List<ProductVariant> existingVariants = productVariantService.getProductVariantByProduct(existingProduct, true);
			for (ProductVariant productVariant : existingVariants) {
				productVariantService.changeStatus(productVariant.getId(), false);
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
		UserLogin userLogin = getUserLoginFromToken();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setVendorId(userLogin.getEntityId());
		} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setActiveRecords(true);
		}
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

	@Override
	public List<CategoryWiseProductCountDTO> getCuisineWiseProductCountList(final Long vendorId) throws NotFoundException {
		List<CategoryWiseProductCountDTO> categoryList = productRepository.getCategoryWiseProductCountList(vendorId, true);
		for (CategoryWiseProductCountDTO categoryWiseProductCountDto : categoryList) {
			CategoryResponseDTO categoryResponseDto = categoryService.getCategory(categoryWiseProductCountDto.getCategoryId());
			categoryWiseProductCountDto.setCategoryName(categoryResponseDto.getName());
		}
		return categoryList;
	}

	@Override
	public List<ProductResponseDTO> getProductListForVendorAndCategory(final Long vendorId, final Long cuisineId)
			throws NotFoundException, ValidationException {
		List<Product> productList = productRepository.findAllByVendorIdAndCategoryId(vendorId, cuisineId);
		List<ProductResponseDTO> productResponseDtoList = new ArrayList<>();
		for (Product product : productList) {
			productResponseDtoList.add(convertEntityToResponseDto(product, true));
		}
		return productResponseDtoList;
	}

	/**
	 * This method will update the rating of the product. Just provide the rating provided by the client and productId.
	 */
	@Override
	public synchronized void updateProductRating(final Long productId, final Double ratingByClient) throws NotFoundException {
		Product product = getProductDetail(productId);
		Double updatedRating = ((product.getRating() * product.getNoOfRating()) + ratingByClient) / (product.getNoOfRating() + 1);
		product.setRating(updatedRating);
		product.setNoOfRating(product.getNoOfRating() + 1);
		productRepository.save(product);
	}
}
