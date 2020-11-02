package com.nice.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.BusinessCategoryDTO;
import com.nice.dto.CategoryResponseDTO;
import com.nice.dto.CategoryWiseProductCountDTO;
import com.nice.dto.CategoryWiseProductReponseDto;
import com.nice.dto.ProductExtrasDTO;
import com.nice.dto.ProductImportDTO;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.SubCategoryWiseProductResponseDto;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductMapper;
import com.nice.model.Brand;
import com.nice.model.CartItem;
import com.nice.model.Category;
import com.nice.model.Cuisine;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.SubCategory;
import com.nice.model.TempCartItem;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.BrandRepository;
import com.nice.repository.CategoryRepository;
import com.nice.repository.CuisineRepository;
import com.nice.repository.ProductRepository;
import com.nice.repository.SubCategoryRepository;
import com.nice.service.AssetService;
import com.nice.service.BrandService;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CartItemService;
import com.nice.service.CategoryService;
import com.nice.service.CuisineService;
import com.nice.service.DiscountService;
import com.nice.service.FileStorageService;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductService;
import com.nice.service.ProductVariantService;
import com.nice.service.SubCategoryService;
import com.nice.service.TempCartItemService;
import com.nice.service.VendorCuisineService;
import com.nice.service.VendorService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service(value = "productService")
@Transactional(rollbackFor = Throwable.class)
public class ProductServiceImpl implements ProductService {
	/**
	 *
	 */
	private static final String ID_NOT_NULL = "product.id.not.null";

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductServiceImpl.class);

	@Autowired
	private ProductRepository productRepository;

	@Autowired
	private ProductMapper productMapper;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private SubCategoryService subCategoryService;

	@Autowired
	private BrandService brandService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private AssetService assetService;

	@Autowired
	private CuisineService cuisineService;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Autowired
	private VendorCuisineService vendorCuisineService;

	@Autowired
	private CartItemService cartItemService;

	@Autowired
	private TempCartItemService tempCartItemService;

	@Autowired
	private DiscountService discountService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private CategoryRepository categoryRepository;

	@Autowired
	private SubCategoryRepository subCategoryRepository;

	@Autowired
	private BrandRepository brandRepository;

	@Autowired
	private CuisineRepository cuisineRepository;

	@Autowired
	private FileStorageService fileStorageService;

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Value("${product.list.image}")
	private String productListImage;

	@Value("${product.detail.image}")
	private String productDetailImage;

	private static final String NON_VEG = "Non_Veg";

	private static final String EGG = "Egg";

	private static final String VEG = "Veg";

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

	@Override
	public boolean isProductExistsEnglish(final ProductRequestDTO productRequestDTO) throws ValidationException {
		LOGGER.info("Inside isExists method, with productReqDto : {}", productRequestDTO);
		UserLogin userLogin = checkForUserLogin();
		productRequestDTO.setVendorId(userLogin.getEntityId());
		if (productRequestDTO.getId() != null) {
			return productRepository.findByNameEnglishIgnoreCaseAndBrandIdAndVendorIdAndIdNot(productRequestDTO.getNameEnglish(),
					productRequestDTO.getBrandId(), productRequestDTO.getVendorId(), productRequestDTO.getId()).isPresent();
		} else {
			return productRepository.findByNameEnglishIgnoreCaseAndBrandIdAndVendorId(productRequestDTO.getNameEnglish(), productRequestDTO.getBrandId(),
					productRequestDTO.getVendorId()).isPresent();
		}
	}

	@Override
	public boolean isProductExistsArabic(final ProductRequestDTO productRequestDTO) throws ValidationException {
		LOGGER.info("Inside isExists method, with productReqDto : {}", productRequestDTO);
		UserLogin userLogin = checkForUserLogin();
		productRequestDTO.setVendorId(userLogin.getEntityId());
		if (productRequestDTO.getId() != null) {
			return productRepository.findByNameArabicIgnoreCaseAndBrandIdAndVendorIdAndIdNot(productRequestDTO.getNameArabic(), productRequestDTO.getBrandId(),
					productRequestDTO.getVendorId(), productRequestDTO.getId()).isPresent();
		} else {
			return productRepository.findByNameArabicIgnoreCaseAndBrandIdAndVendorId(productRequestDTO.getNameArabic(), productRequestDTO.getBrandId(),
					productRequestDTO.getVendorId()).isPresent();
		}
	}

	@Override
	public void addProduct(final ProductRequestDTO productRequestDTO, final MultipartFile image, final MultipartFile detailImage)
			throws NotFoundException, ValidationException, FileOperationException {

		VendorResponseDTO vendorDto = vendorService.getVendor(productRequestDTO.getVendorId());

		LOGGER.info("Inside addProduct method, with productReqDto : {}", productRequestDTO);
		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO);
		/**
		 * Set some default values when product is created
		 */
		product.setRating(0.0);
		product.setNoOfRating(0L);
		product.setBusinessCategoryId(vendorDto.getBusinessCategoryId());
		if (image != null) {
			uploadImage(image, product);
		} else {
			product.setImage(productListImage);
			product.setImageOriginalName(productListImage);
		}
		if (detailImage != null) {
			uploadDetailImage(detailImage, product);
		} else {
			product.setDetailImage(productDetailImage);
			product.setDetailImageOriginalName(productDetailImage);
		}
		productRepository.save(product);
		LOGGER.info("addProduct executed successfully");
	}

	@Override
	public void updateProduct(final ProductRequestDTO productRequestDTO, final MultipartFile image, final MultipartFile detailImage)
			throws NotFoundException, ValidationException, FileOperationException {
		LOGGER.info("Inside updateProduct method, with productReqDto : {}", productRequestDTO);
		if (productRequestDTO.getId() == null) {
			LOGGER.error("Error in updating product , id is null");
			throw new ValidationException(messageByLocaleService.getMessage(ID_NOT_NULL, null));
		}
		final Product existingProduct = getProductDetail(productRequestDTO.getId());
		productRequestDTO.setVendorId(existingProduct.getVendorId());
		validationForProduct(productRequestDTO);
		Product product = productMapper.toEntity(productRequestDTO);
		product.setRating(existingProduct.getRating());
		product.setNoOfRating(existingProduct.getNoOfRating());
		product.setBusinessCategoryId(existingProduct.getBusinessCategoryId());
		updateImages(image, existingProduct, product);
		updateDetailImage(detailImage, existingProduct, product);
		productRepository.save(product);
		LOGGER.info("updateProduct executed successfully");
	}

	private void updateImages(final MultipartFile image, final Product existingProduct, final Product product)
			throws FileOperationException, ValidationException {
		LOGGER.info("Inside image for Product : {}", product.getId());
		if (image != null && !existingProduct.getImage().equals(productListImage)) {
			deleteOldImage(existingProduct.getImage());
			uploadImage(image, product);
		} else {
			product.setImage(existingProduct.getImage());
			product.setImageOriginalName(existingProduct.getImageOriginalName());
		}
		LOGGER.info("After updating image for Product : {}", product.getId());
	}

	private void updateDetailImage(final MultipartFile detailImage, final Product existingProduct, final Product product)
			throws FileOperationException, ValidationException {
		LOGGER.info("Inside image for Product : {}", product.getId());
		if (detailImage != null && !existingProduct.getDetailImage().equals(productDetailImage)) {
			deleteOldImage(existingProduct.getDetailImage());
			uploadDetailImage(detailImage, product);
		} else {
			product.setDetailImage(existingProduct.getDetailImage());
			product.setDetailImageOriginalName(existingProduct.getDetailImageOriginalName());
		}
		LOGGER.info("After updating image for Product : {}", product.getId());
	}

	@SuppressWarnings("unchecked")
	private Map<Long, List<? extends Object>> getCartMap(final Long customerId, final String uuid) throws ValidationException, NotFoundException {
		Map<Long, List<?>> cartMap = new HashMap<>();
		LOGGER.info("Inside getCart for Customer : {} and uuid :{}", customerId, uuid);
		if (customerId != null) {
			List<CartItem> cartItemResponse = cartItemService.getCartListBasedOnCustomer(customerId);
			for (CartItem cartItem : cartItemResponse) {
				List<?> objectList = cartMap.get(cartItem.getProductVariant().getId());
				if (objectList == null) {
					List<CartItem> cartItemList = new ArrayList<>();
					cartItemList.add(cartItem);
					cartMap.put(cartItem.getProductVariant().getId(), cartItemList);
				} else {
					((List<CartItem>) objectList).add(cartItem);
				}
			}
		} else if (uuid != null) {
			List<TempCartItem> tempCartItemList = tempCartItemService.getCartListBasedOnUuid(uuid);
			for (TempCartItem tempCartItem : tempCartItemList) {
				List<?> objectList = cartMap.get(tempCartItem.getProductVariant().getId());
				if (objectList == null) {
					List<TempCartItem> cartItemList = new ArrayList<>();
					cartItemList.add(tempCartItem);
					cartMap.put(tempCartItem.getProductVariant().getId(), cartItemList);
				} else {
					((List<TempCartItem>) objectList).add(tempCartItem);
				}
			}
		}
		LOGGER.info("After getCart for Customer : {} and uuid :{}", customerId, uuid);
		return cartMap;
	}

	@Override
	public ProductResponseDTO getProduct(final Long productId, final String uuid) throws NotFoundException, ValidationException {
		LOGGER.info("Inside getProduct for Product : {} and uuid :{}", productId, uuid);
		Boolean isAdmin = true;
		UserLogin userLogin = getUserLoginFromToken();
		Map<Long, List<? extends Object>> cartMap = null;
		if (userLogin == null) {
			isAdmin = false;
			cartMap = getCartMap(null, uuid);
		} else if (UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			isAdmin = false;
			cartMap = getCartMap(userLogin.getEntityId(), null);
		}
		Product product = productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));
		LOGGER.info("After getProduct for Product : {} and uuid :{}", productId, uuid);
		return convertEntityToResponseDto(product, isAdmin, cartMap);

	}

	@Override
	public Product getProductDetail(final Long productId) throws NotFoundException {
		LOGGER.info("Inside getProductDetail for Product : {} ", productId);
		return productRepository.findById(productId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("product.not.found", new Object[] { productId })));
	}

	@Override
	public List<ProductResponseDTO> getProductListBasedOnParams(final ProductParamRequestDTO productParamRequestDTO, final Integer startIndex,
			final Integer pageSize) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get product list Based On Params {}", productParamRequestDTO);
		UserLogin userLogin = getUserLoginFromToken();
		Boolean listForAdmin = true;
		Map<Long, List<? extends Object>> cartMap = null;
		if (userLogin != null && (UserType.VENDOR.name().equals(userLogin.getEntityType()))) {
			productParamRequestDTO.setVendorId(userLogin.getEntityId());
		} else if (userLogin == null || UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			cartMap = userLogin == null ? getCartMap(null, productParamRequestDTO.getUuid()) : getCartMap(userLogin.getEntityId(), null);
			productParamRequestDTO.setActiveRecords(true);
			listForAdmin = false;
		}
		productParamRequestDTO.setFromAdmin(listForAdmin);
		List<ProductResponseDTO> responseDTOs = new ArrayList<>();
		List<Product> products = productRepository.getProductListBasedOnParams(productParamRequestDTO, startIndex, pageSize);
		for (Product product : products) {
			final ProductResponseDTO responseDTO = convertEntityToResponseDto(product, listForAdmin, cartMap);
			if (listForAdmin || responseDTO.getProductAvailable()) {
				responseDTOs.add(responseDTO);
			}
		}
		LOGGER.info("After getProductListBasedOnParams ");
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
		LOGGER.info("Inside validationForProduct for ProductRequestDto : {}", productRequestDTO);
		/**
		 * If the login user is not a vendor then throw an exception
		 */
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productRequestDTO.getId() != null && !productRequestDTO.getVendorId().equals(userLogin.getEntityId())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		/**
		 * vendor's profile is not completed yet then throw an exception
		 */
		Vendor vendor = vendorService.getVendorDetail(productRequestDTO.getVendorId());
		if (!vendor.getProfileCompleted().booleanValue()) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.profile.incomplete", null));
		}

		if (!Arrays.asList(0, 1, 2).contains(productRequestDTO.getProductFoodType())) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.product.food.type", null));
		}

		/**
		 * for validation of made product foreign keys
		 */
		categoryService.getCategoryDetail(productRequestDTO.getCategoryId());
		subCategoryService.getSubCategoryDetail(productRequestDTO.getSubcategoryId());
		if (Constant.BUSINESS_CATEGORY_FOOD_ENGLISH.equals(vendor.getBusinessCategory().getNameEnglish())) {
			if (productRequestDTO.getCuisineId() == null) {
				throw new ValidationException(messageByLocaleService.getMessage("cuisine.required", null));
			} else {
				cuisineService.getCuisineDetails(productRequestDTO.getCuisineId());
				vendorCuisineService.getVendorCuisineByVendorIdAndCuisineId(productRequestDTO.getVendorId(), productRequestDTO.getCuisineId());
			}
		}

		LOGGER.info("After validationForProduct for ProductRequestDto : {}", productRequestDTO);
	}

	@Override
	public List<CategoryWiseProductReponseDto> getProductListBasedOnParamsAndCategoryWise(final ProductParamRequestDTO productParamRequestDTO)
			throws NotFoundException, ValidationException {

		LOGGER.info("Inside getProductListBasedOnParamsAndCategoryWise");
		List<ProductResponseDTO> productResponseDtoList = getProductListBasedOnParams(productParamRequestDTO, null, null);
		/**
		 * Set category based product list
		 */
		Map<String, Map<String, List<ProductResponseDTO>>> productResponseDtoListCategoryWise = new HashMap<>();
		for (ProductResponseDTO productResponseDto : productResponseDtoList) {
			/**
			 * If category does not exists in Map
			 */
			if (productResponseDtoListCategoryWise.get(productResponseDto.getCategoryName()) == null) {
				List<ProductResponseDTO> prodRespDtoList = new ArrayList<>();
				prodRespDtoList.add(productResponseDto);
				Map<String, List<ProductResponseDTO>> subCategoryWiseProductResponseMap = new HashMap<>();
				subCategoryWiseProductResponseMap.put(productResponseDto.getSubcategoryName(), prodRespDtoList);
				productResponseDtoListCategoryWise.put(productResponseDto.getCategoryName(), subCategoryWiseProductResponseMap);
			}
			/**
			 * If category exists but subcategory related details exists or not exists
			 */
			else {
				Map<String, List<ProductResponseDTO>> subCategoryWiseProductMap = productResponseDtoListCategoryWise.get(productResponseDto.getCategoryName());
				if (subCategoryWiseProductMap.get(productResponseDto.getSubcategoryName()) == null) {
					List<ProductResponseDTO> prodRespDtoList = new ArrayList<>();
					prodRespDtoList.add(productResponseDto);
					subCategoryWiseProductMap.put(productResponseDto.getSubcategoryName(), prodRespDtoList);
					productResponseDtoListCategoryWise.put(productResponseDto.getCategoryName(), subCategoryWiseProductMap);
				} else {
					subCategoryWiseProductMap.get(productResponseDto.getSubcategoryName()).add(productResponseDto);
				}
			}
		}
		LOGGER.info("After getProductListBasedOnParamsAndCategoryWise ");
		/**
		 * Convert Map to List of Dto
		 */
		List<CategoryWiseProductReponseDto> categoryWiseProductResponseDtoList = new ArrayList<>();
		for (Entry<String, Map<String, List<ProductResponseDTO>>> categoryEntrySet : productResponseDtoListCategoryWise.entrySet()) {
			CategoryWiseProductReponseDto categoryWiseProductResponseDto = new CategoryWiseProductReponseDto();
			List<SubCategoryWiseProductResponseDto> subcategoryWiseProductResponseDto = new ArrayList<>();
			for (Entry<String, List<ProductResponseDTO>> subcategoryEntry : categoryEntrySet.getValue().entrySet()) {
				SubCategoryWiseProductResponseDto subCategoryWiseProductResponseDto = new SubCategoryWiseProductResponseDto();
				List<ProductResponseDTO> subCateogryWiseProductResponseDtoList = subcategoryEntry.getValue();
				subCategoryWiseProductResponseDto.setSubCategoryName(subcategoryEntry.getKey());
				subCategoryWiseProductResponseDto.setProductResponseList(subCateogryWiseProductResponseDtoList);
				subcategoryWiseProductResponseDto.add(subCategoryWiseProductResponseDto);
			}
			categoryWiseProductResponseDto.setCategoryName(categoryEntrySet.getKey());
			categoryWiseProductResponseDto.setSubcateogryList(subcategoryWiseProductResponseDto);
			categoryWiseProductResponseDtoList.add(categoryWiseProductResponseDto);
		}
		LOGGER.info("After converting to response from map to object list ");
		return categoryWiseProductResponseDtoList;
	}

	@Override
	public List<ProductResponseDTO> getProductDetailList(final List<Product> products) throws NotFoundException, ValidationException {
		LOGGER.info("Inside getProductDetailList");
		List<ProductResponseDTO> productResponseDTOs = new ArrayList<>();
		for (Product product : products) {
			productResponseDTOs.add(convertEntityToResponseDto(product, false, null));
		}
		LOGGER.info("After getProductDetailList");
		return productResponseDTOs;
	}

	/**
	 * listForAdmin==null means get product detail for admin listForAdmin==true means get product list for admin convert
	 * entity to response dto
	 *
	 * @param product
	 * @param listForAdmin
	 * @param productParamRequestDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	private ProductResponseDTO convertEntityToResponseDto(final Product product, final Boolean listForAdmin, final Map<Long, List<? extends Object>> cartMap)
			throws NotFoundException, ValidationException {
		/**
		 * set foreign key values to response DTO
		 */
		Locale locale = LocaleContextHolder.getLocale();
		LOGGER.info("Inside convertEntityToResponseDto");
		ProductResponseDTO productResponseDTO = productMapper.toResponseDto(product);

		BusinessCategoryDTO businessCategoryDto = businessCategoryService.getBusinessCategory(product.getBusinessCategoryId());
		productResponseDTO.setBusinessCategoryNameEnglish(businessCategoryDto.getNameEnglish());
		productResponseDTO.setBusinessCategoryNameArabic(businessCategoryDto.getNameArabic());
		Category category = categoryService.getCategoryDetail(productResponseDTO.getCategoryId());
		productResponseDTO.setManageInventory(businessCategoryDto.getManageInventory());

		/**
		 * Get Vendor details
		 */

		Vendor vendor = vendorService.getVendorDetail(product.getVendorId());

		productResponseDTO.setStoreNameArabic(vendor.getStoreNameArabic());
		productResponseDTO.setStoreNameEnglish(vendor.getStoreNameEnglish());

		productResponseDTO.setCategoryNameEnglish(category.getNameEnglish());
		productResponseDTO.setCategoryNameArabic(category.getNameArabic());
		if (productResponseDTO.getSubcategoryId() != null) {
			SubCategory subCategory = subCategoryService.getSubCategoryDetail(productResponseDTO.getSubcategoryId());
			productResponseDTO.setSubcategoryNameEnglish(subCategory.getNameEnglish());
			productResponseDTO.setSubcategoryNameArabic(subCategory.getNameArabic());
		}
		if (productResponseDTO.getCuisineId() != null) {
			Cuisine cuisine = cuisineService.getCuisineDetails(productResponseDTO.getCuisineId());
			productResponseDTO.setCuisineNameEnglish(cuisine.getNameEnglish());
			productResponseDTO.setCuisineNameArabic(cuisine.getNameArabic());

		}
		if (productResponseDTO.getBrandId() != null) {
			Brand brand = brandService.getBrandDetail(productResponseDTO.getBrandId());
			productResponseDTO.setBrandNameEnglish(brand.getNameEnglish());
			productResponseDTO.setBrandNameArabic(brand.getNameArabic());
		}

		if (locale.getLanguage().equals("en")) {
			productResponseDTO.setCategoryName(productResponseDTO.getCategoryNameEnglish());
			productResponseDTO.setSubcategoryName(productResponseDTO.getSubcategoryNameEnglish());
			productResponseDTO.setCuisineName(productResponseDTO.getCuisineNameEnglish());
			productResponseDTO.setBrandName(productResponseDTO.getBrandNameEnglish());
			productResponseDTO.setBusinessCategoryName(productResponseDTO.getBusinessCategoryNameEnglish());
			productResponseDTO.setStoreName(vendor.getStoreNameEnglish());
			productResponseDTO.setStoreContactNumber(vendor.getStorePhoneNumber());
		} else {
			productResponseDTO.setCategoryName(productResponseDTO.getCategoryNameArabic());
			productResponseDTO.setSubcategoryName(productResponseDTO.getSubcategoryNameArabic());
			productResponseDTO.setCuisineName(productResponseDTO.getCuisineNameArabic());
			productResponseDTO.setBrandName(productResponseDTO.getBrandNameArabic());
			productResponseDTO.setBusinessCategoryName(productResponseDTO.getBusinessCategoryNameArabic());
			productResponseDTO.setStoreName(vendor.getStoreNameArabic());
			productResponseDTO.setStoreContactNumber(vendor.getStorePhoneNumber());
		}
		productResponseDTO.setImage(assetService.getGeneratedUrl(product.getImage(), AssetConstant.PRODUCT_DIR));
		productResponseDTO.setDetailImage(assetService.getGeneratedUrl(product.getDetailImage(), AssetConstant.PRODUCT_DIR));
		/**
		 * if we are fetching product list For admin then set product variants to empty list
		 */
		List<ProductVariantResponseDTO> productVariantList = new ArrayList<>();

		if (listForAdmin != null && listForAdmin.booleanValue()) {
			productResponseDTO.setProductVariantList(Collections.emptyList());
		} else {
			/**
			 * If the list is not for admin then display only active records
			 */
			productVariantList = productVariantService.getProductVariantDetailByProduct(product, true, listForAdmin);
			productResponseDTO.setProductAvailable(false);
		}
		LOGGER.info("Before setting available qty");

		Integer availableQty = 0;
		/**
		 * This variable will be used only to decide if the product will be visible on front end.
		 */

		for (ProductVariantResponseDTO productVariantResponseDTO : productVariantList) {
			availableQty += productVariantResponseDTO.getAvailableQty() != null ? productVariantResponseDTO.getAvailableQty() : 0;
			productResponseDTO.setProductAvailable(true);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_MAP.test(cartMap)) {
				List<? extends Object> obj = cartMap.get(productVariantResponseDTO.getId());
				if (obj != null && !obj.isEmpty()) {
					for (Object o : obj) {
						if (o instanceof CartItem) {
							CartItem cartItem = (CartItem) o;
							productVariantResponseDTO.getCartQtyList().add(cartItem.getQuantity());
							productVariantResponseDTO.getCartIdList().add(cartItem.getId());
							productResponseDTO.setCartQty(productResponseDTO.getCartQty() + cartItem.getQuantity());
						} else if (o instanceof TempCartItem) {
							TempCartItem tempCartItem = (TempCartItem) o;
							productVariantResponseDTO.getCartQtyList().add(tempCartItem.getQuantity());
							productVariantResponseDTO.getCartIdList().add(tempCartItem.getId());
							productResponseDTO.setCartQty(productResponseDTO.getCartQty() + tempCartItem.getQuantity());
						}
					}
				}
			}
		}

		productResponseDTO.setProductOutOfStock(false);
		/**
		 * Here flag is set to determine whether to make product visible to customer or not, if the setProductAvailable flag is
		 * true then product should be made visible else that product should be shown as out of stock
		 */
		if (!businessCategoryDto.getNameEnglish().equalsIgnoreCase(Constant.BUSINESS_CATEGORY_FOOD_DELIVERY)) {
			productResponseDTO.setProductOutOfStock(availableQty <= 0);
		}

		productResponseDTO.setProductVariantList(CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productVariantList) ? productVariantList : Collections.emptyList());
		productResponseDTO.setProductExtrasList(productExtrasService.getList(listForAdmin ? null : true, product.getId()));
		LOGGER.info("Inside convertEntityToResponseDto");
		if (product.getDiscountId() != null) {
			productResponseDTO.setDiscountStatus(discountService.getDiscountDetails(product.getDiscountId()).getStatus());
		}

		return productResponseDTO;
	}

	@Override
	public void changeStatus(final Long productId, final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside changeStatus for Product :{}, status :{}", productId, active);
		Product product = getProductDetail(productId);
		UserLogin userLogin = checkForUserLogin();
		/**
		 * Only the vendor who created the produce and the admin can deactivate the product
		 */
		if (!((UserType.VENDOR.name().equals(userLogin.getEntityType()) && product.getVendorId().equals(userLogin.getEntityId()))
				|| userLogin.getEntityType() == null)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (product.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage("product.active.deactive", new Object[] {
					(Boolean.TRUE.equals(active) ? messageByLocaleService.getMessage("active", null) : messageByLocaleService.getMessage("deActive", null)) }));
		} else {
			changeStatusForDependentEntity(active, product);
			product.setActive(active);
			productRepository.save(product);
		}
		LOGGER.info("After changeStatus for Product :{}, status :{}", productId, active);
	}

	/**
	 * @param active
	 * @param existingProduct
	 * @param productId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	public void changeStatusForDependentEntity(final Boolean active, final Product existingProduct) throws NotFoundException, ValidationException {
		LOGGER.info("Inside changeStatusForDependentEntity for Product :{}, status :{}", existingProduct.getId(), active);
		if (Boolean.FALSE.equals(active)) {
			/**
			 * if active is false then deactivate its variants.
			 */
			List<ProductVariant> existingVariants = productVariantService.getProductVariantByProduct(existingProduct, true);
			for (ProductVariant productVariant : existingVariants) {
				productVariantService.changeStatus(productVariant.getId(), false);
			}
			/**
			 * deactivate product extras of this product
			 */
			List<ProductExtrasDTO> productExtrasList = productExtrasService.getList(true, existingProduct.getId());
			for (ProductExtrasDTO productExtrasDTO : productExtrasList) {
				productExtrasService.changeStatus(productExtrasDTO.getId(), false);
			}
		} else {
			validationForActivateProduct(existingProduct);
		}
		LOGGER.info("After changeStatusForDependentEntity for Product :{}, status :{}", existingProduct.getId(), active);
	}

	/**
	 * check masters for activate product
	 *
	 * @param existingProduct
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void validationForActivateProduct(final Product existingProduct) throws NotFoundException, ValidationException {
		LOGGER.info("Inside validationForActivateProduct for Product :{}", existingProduct.getId());
		if (Boolean.FALSE.equals(categoryService.getCategoryDetail(existingProduct.getCategoryId()).getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("category.activate.first.product", null));
		}
		if (Boolean.FALSE.equals(subCategoryService.getSubCategoryDetail(existingProduct.getSubcategoryId()).getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("subcategory.activate.first", null));
		}

		LOGGER.info("After validationForActivateProduct for Product :{}", existingProduct.getId());
	}

	@Override
	public List<Product> getProductListBasedOnParamsWithoutPagination(final ProductParamRequestDTO productParamRequestDTO) {
		LOGGER.info("Inside getProductListBasedOnParamsWithoutPagination for ProductParamRequestDTO :{}", productParamRequestDTO);
		return productRepository.getProductListBasedOnParams(productParamRequestDTO, null, null);
	}

	@Override
	public Long getProductCountBasedOnParams(final ProductParamRequestDTO productParamRequestDTO) throws NotFoundException {
		LOGGER.info("Inside getProductCountBasedOnParams for ProductParamRequestDTO :{}", productParamRequestDTO);

		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * In case of customer the userLogin might be anonymous user, resulting in no user login and hence if the userLogin is
		 * null or the userLogin->entityType is customer then we will give records specific to customer
		 */
		Boolean listForAdmin = true;
		if (userLogin != null && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setVendorId(userLogin.getEntityId());
		} else if (userLogin == null || UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			productParamRequestDTO.setActiveRecords(true);
			listForAdmin = false;
		}
		productParamRequestDTO.setFromAdmin(listForAdmin);
		return productRepository.getProductCountBasedOnParams(productParamRequestDTO);
	}

	/**
	 * upload image of product
	 *
	 * @param image
	 * @param product
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	private void uploadImage(final MultipartFile image, final Product product) throws FileOperationException, ValidationException {
		LOGGER.info("Inside uploadImage for product :{}", product.getId());
		product.setImage(assetService.saveAsset(image, AssetConstant.PRODUCT_DIR, 0, Constant.PRODUCT_LIST_IMAGE_WIDTH, Constant.PRODUCT_LIST_IMAGE_HEIGHT));
		product.setImageOriginalName(image.getOriginalFilename());
		LOGGER.info("After uploadImage for product :{}", product.getId());
	}

	private void uploadDetailImage(final MultipartFile detailImage, final Product product) throws FileOperationException, ValidationException {
		LOGGER.info("Inside uploadImage for product :{}", product.getId());
		product.setDetailImage(
				assetService.saveAsset(detailImage, AssetConstant.PRODUCT_DIR, 0, Constant.PRODUCT_DETAIL_IMAGE_WIDTH, Constant.PRODUCT_DETAIL_IMAGE_HEIGHT));
		product.setDetailImageOriginalName(detailImage.getOriginalFilename());
		LOGGER.info("After uploadImage for product :{}", product.getId());
	}

	/**
	 * delete old image
	 *
	 * @param product
	 */
	private void deleteOldImage(final String ImageName) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(ImageName)) {
			LOGGER.info("Inside deleteOldImage for product :{}", ImageName);
			assetService.deleteFile(ImageName, AssetConstant.PRODUCT_DIR);
			LOGGER.info("After deleteOldImage for product :{}", ImageName);
		}
	}

	@Override
	public List<CategoryWiseProductCountDTO> getCuisineWiseProductCountList(final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside getCuisineWiseProductCountList for vendor :{}", vendorId);
		List<CategoryWiseProductCountDTO> categoryList = productRepository.getCategoryWiseProductCountList(vendorId, true);
		for (CategoryWiseProductCountDTO categoryWiseProductCountDto : categoryList) {
			CategoryResponseDTO categoryResponseDto = categoryService.getCategory(categoryWiseProductCountDto.getCategoryId());
			categoryWiseProductCountDto.setCategoryName(categoryResponseDto.getName());
		}
		LOGGER.info("After getCuisineWiseProductCountList for vendor :{}", vendorId);
		return categoryList;
	}

	@Override
	public List<ProductResponseDTO> getProductListForVendorAndCategory(final Long vendorId, final Long categoryId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside getCuisineWiseProductCountList for vendor :{} and categpry :{}", vendorId, categoryId);
		List<Product> productList = productRepository.findAllByVendorIdAndCategoryId(vendorId, categoryId);
		List<ProductResponseDTO> productResponseDtoList = new ArrayList<>();
		for (Product product : productList) {
			productResponseDtoList.add(convertEntityToResponseDto(product, true, null));
		}
		LOGGER.info("After getCuisineWiseProductCountList for vendor :{} and categpry :{}", vendorId, categoryId);
		return productResponseDtoList;
	}

	@Override
	public List<ProductResponseDTO> getProductListForVendorAndCuisine(final Long vendorId, final Long cuisineId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside getCuisineWiseProductCountList for vendor :{} and cuisine :{}", vendorId, cuisineId);
		List<Product> productList = productRepository.findAllByVendorIdAndCuisineId(vendorId, cuisineId);
		List<ProductResponseDTO> productResponseDtoList = new ArrayList<>();
		for (Product product : productList) {
			productResponseDtoList.add(convertEntityToResponseDto(product, true, null));
		}
		LOGGER.info("Inside getCuisineWiseProductCountList for vendor :{} and cuisine :{}", vendorId, cuisineId);
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

	@Override
	public void exportProductList(final HttpServletResponse httpServletResponse, final ProductParamRequestDTO productParamRequestDTO)
			throws NotFoundException, ValidationException, FileNotFoundException {
		List<ProductResponseDTO> responseDTOs = new ArrayList<>();
		LOGGER.info("Inside export product list Based On Params {}", productParamRequestDTO);
		List<Product> products = productRepository.getProductListBasedOnParams(productParamRequestDTO, null, null);
		for (Product product : products) {
			ProductResponseDTO responseDTO = convertEntityToResponseDto(product, false, null);
			responseDTOs.add(responseDTO);
		}
		final Object[] productHeaderField = new Object[] { "Name", "Category", "Sub Category", "Image" };
		final Object[] productDataField = new Object[] { "name", "categoryName", "subcategoryName", "image" };
		try {
			exportCSV.writeCSVFile(responseDTOs, productDataField, productHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "product", AssetConstant.PRODUCT_DIR);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.PRODUCT_DIR);
		final File file = new File(filePath.toString());
		final CSVProcessor<ProductImportDTO> csvProcessor = new CSVProcessor<>();
		try {
			final List<ProductImportDTO> productImportDTOs = csvProcessor.convertCSVFileToListOfBean(file, ProductImportDTO.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productImportDTOs)) {
				final List<ProductImportDTO> insertListOfBean = insertListOfProducts(productImportDTOs);
				Object[] productDetailsHeadersField = new Object[] { "Product Name English", "Product Name Arabic", "Result" };
				Object[] productDetailsField = new Object[] { "nameEnglish", "nameArabic", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, productDetailsField, productDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}
	}

	/**
	 * @param productImportDTOs
	 * @param userId
	 * @return
	 */
	private List<ProductImportDTO> insertListOfProducts(final List<ProductImportDTO> productImportDTOs) {
		final List<ProductImportDTO> allResult = new ArrayList<>();
		for (ProductImportDTO productImportDTO : productImportDTOs) {
			try {
				ProductRequestDTO productRequestDTO = validationForProductImport(productImportDTO);
				addProduct(productRequestDTO, null, null);
				productImportDTO.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
			} catch (Exception e) {
				productImportDTO.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(productImportDTO);
		}
		return allResult;

	}

	private ProductRequestDTO validationForProductImport(final ProductImportDTO productImportDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside validation for product import for productImportDTO : {}", productImportDTO);
		Long brandId = null;
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());

		final ProductRequestDTO productRequestDTO = new ProductRequestDTO();
		BeanUtils.copyProperties(productImportDTO, productRequestDTO);
		productRequestDTO.setVendorId(vendor.getId());
		productRequestDTO.setActive(true);

		if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getNameEnglish())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.name.english.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getNameArabic())) {
			throw new ValidationException(messageByLocaleService.getMessage("product.name.arabic.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCategoryNameEnglish())) {
			throw new ValidationException(messageByLocaleService.getMessage("category.name.english.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCategoryNameArabic())) {
			throw new ValidationException(messageByLocaleService.getMessage("category.name.arabic.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getDescriptionEnglish())) {
			throw new ValidationException(messageByLocaleService.getMessage("description.english.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getDescriptionArabic())) {
			throw new ValidationException(messageByLocaleService.getMessage("description.arabic.not.null", null));
		} else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getProductFoodType())) {
			throw new ValidationException(messageByLocaleService.getMessage("food.type.not.null", null));
		} else if (productImportDTO.getVendorId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.null", null));
		} else if (!(VEG.equalsIgnoreCase(productImportDTO.getProductFoodType()) || NON_VEG.equalsIgnoreCase(productImportDTO.getProductFoodType())
				|| EGG.equalsIgnoreCase(productImportDTO.getProductFoodType()))) {
			throw new ValidationException(messageByLocaleService.getMessage("food.type.not.proper", new Object[] { messageByLocaleService.getMessage(VEG, null),
					messageByLocaleService.getMessage(NON_VEG, null), messageByLocaleService.getMessage(EGG, null) }));
		} else if (Constant.BUSINESS_CATEGORY_FOOD_ENGLISH.equals(vendor.getBusinessCategory().getNameEnglish())
				&& !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCuisineNameEnglish())) {
			throw new ValidationException(messageByLocaleService.getMessage("cuisine.english.required", null));
		} else if (Constant.BUSINESS_CATEGORY_FOOD_ENGLISH.equals(vendor.getBusinessCategory().getNameEnglish())
				&& !CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCuisineNameArabic())) {
			throw new ValidationException(messageByLocaleService.getMessage("cuisine.arabic.required", null));
		}
		if (VEG.equalsIgnoreCase(productImportDTO.getProductFoodType())) {
			productRequestDTO.setProductFoodType(1);
		} else if (NON_VEG.equalsIgnoreCase(productImportDTO.getProductFoodType())) {
			productRequestDTO.setProductFoodType(2);
		} else {
			productRequestDTO.setProductFoodType(3);
		}
		validateAndSetProductImport(productImportDTO, brandId, vendor, productRequestDTO);
		return productRequestDTO;
	}

	/**
	 * @param productImportDTO
	 * @param brandId
	 * @param vendor
	 * @param productRequestDTO
	 * @throws ValidationException
	 */
	private void validateAndSetProductImport(final ProductImportDTO productImportDTO, final Long brandId, final Vendor vendor,
			final ProductRequestDTO productRequestDTO) throws ValidationException {
		Optional<Category> category = categoryRepository.findByNameEnglishIgnoreCaseAndNameArabicIgnoreCaseAndVendor(productImportDTO.getCategoryNameEnglish(),
				productImportDTO.getCategoryNameArabic(), vendor);
		if (!category.isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("category.name.english.arabic.not.found",
					new Object[] { productImportDTO.getCategoryNameEnglish(), productImportDTO.getCategoryNameArabic() }));
		} else {
			productRequestDTO.setCategoryId(category.get().getId());
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getSubcategoryNameEnglish())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getSubcategoryNameArabic())) {
			Optional<SubCategory> subCategory = subCategoryRepository.findByNameEnglishIgnoreCaseAndNameArabicIgnoreCaseAndCategory(
					productImportDTO.getSubcategoryNameEnglish(), productImportDTO.getSubcategoryNameArabic(), category.get());
			if (!subCategory.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("subcategory.name.english.arabic.not.found",
						new Object[] { productImportDTO.getSubcategoryNameEnglish(), productImportDTO.getSubcategoryNameArabic() }));
			} else {
				productRequestDTO.setSubcategoryId(subCategory.get().getId());
			}
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCuisineNameEnglish())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getCuisineNameArabic())) {
			Optional<Cuisine> cuisine = cuisineRepository.findByNameEnglishIgnoreCaseAndNameArabicIgnoreCase(productImportDTO.getCuisineNameEnglish(),
					productImportDTO.getCuisineNameArabic());
			if (!cuisine.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("cuisine.name.english.arabic.not.found",
						new Object[] { productImportDTO.getCuisineNameEnglish(), productImportDTO.getCuisineNameArabic() }));
			} else {
				productRequestDTO.setCuisineId(cuisine.get().getId());
			}
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getBrandNameEnglish())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(productImportDTO.getBrandNameArabic())) {
			Optional<Brand> brand = brandRepository.findByNameEnglishIgnoreCaseAndNameArabicIgnoreCase(productImportDTO.getBrandNameEnglish(),
					productImportDTO.getBrandNameArabic());
			if (!brand.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("brand.name.english.arabic.not.found",
						new Object[] { productImportDTO.getBrandNameEnglish(), productImportDTO.getBrandNameArabic() }));
			} else {
				productRequestDTO.setBrandId(brand.get().getId());
			}
		}
		if (productRepository.findByNameEnglishIgnoreCaseAndBrandIdAndVendorId(productImportDTO.getNameEnglish(), brandId, vendor.getId()).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("english.product.already.exists", null));
		}
		if (productRepository.findByNameArabicIgnoreCaseAndBrandIdAndVendorId(productImportDTO.getNameArabic(), brandId, vendor.getId()).isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("arabic.product.already.exists", null));
		}
	}

	@Override
	public void deleteImage(final String imageType, final Long productId) throws NotFoundException, ValidationException {
		Product product = getProductDetail(productId);
		if (product.getDetailImage().equals(productDetailImage) || product.getImage().equals(productListImage)) {
			throw new ValidationException(messageByLocaleService.getMessage("default.image.can.not.delete", null));
		}
		if (imageType.equals("DETAIL")) {
			deleteOldImage(product.getDetailImage());
			product.setDetailImage(null);
			product.setDetailImageOriginalName(null);
		} else if (imageType.equals("LIST")) {
			deleteOldImage(product.getImage());
			product.setImage(null);
			product.setImageOriginalName(null);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("image.type.not.valid", null));
		}
		productRepository.save(product);
	}
}
