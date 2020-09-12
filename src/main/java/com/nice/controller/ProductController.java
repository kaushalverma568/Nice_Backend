package com.nice.controller;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CategoryWiseProductCountDTO;
import com.nice.dto.CategoryWiseProductReponseDto;
import com.nice.dto.PaginationUtilDto;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.exception.BaseException;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.ProductService;
import com.nice.util.PaginationUtil;
import com.nice.validator.ProductValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@RequestMapping(path = "/product")
@RestController(value = "productController")
public class ProductController {
	/**
	 *
	 */
	private static final String PRODUCT_CREATE_MESSAGE = "product.create.message";
	/**
	 *
	 */
	private static final String PRODUCT_LIST_MESSAGE = "product.list.message";
	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ProductController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding made product
	 */
	@Autowired
	private ProductValidator productValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(productValidator);
	}

	@Autowired
	private ProductService productService;

	public static final String IMAGE_NOT_NULL = "image.required";

	/**
	 * @param  accessToken
	 * @param  image
	 * @param  userId
	 * @param  productRequestDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@PostMapping()
	@PreAuthorize("hasPermission('Product List','CAN_ADD')")
	public ResponseEntity<Object> addProduct(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "image", required = true) final MultipartFile image,
			@RequestParam(name = "detailImage", required = true) final MultipartFile detailImage,
			@ModelAttribute @Valid final ProductRequestDTO productRequestDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add Product {}", productRequestDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Product validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productService.addProduct(productRequestDTO, image, detailImage);
		LOGGER.info("Outside add Product");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_CREATE_MESSAGE, null))
				.create();
	}

	/**
	 * @param  accessToken
	 * @param  image
	 * @param  userId
	 * @param  productRequestDTO
	 * @param  result
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	@Consumes(MediaType.MULTIPART_FORM_DATA)
	@Produces(MediaType.APPLICATION_JSON)
	@PutMapping()
	@PreAuthorize("hasPermission('Product List','CAN_EDIT')")
	public ResponseEntity<Object> updateProduct(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "image", required = false) final MultipartFile image,
			@RequestParam(name = "detailImage", required = false) final MultipartFile detailImage,
			@ModelAttribute @Valid final ProductRequestDTO productRequestDTO, final BindingResult result) throws ValidationException, NotFoundException {
		LOGGER.info("Inside update Product {}", productRequestDTO);
		final List<FieldError> fieldErrors = result.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			LOGGER.error("Product validation failed");
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		productService.updateProduct(productRequestDTO, image, detailImage);
		LOGGER.info("Outside update Product");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.update.message", null))
				.create();
	}

	/**
	 * @param  productId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@GetMapping("/{productId}")
	public ResponseEntity<Object> getProduct(@PathVariable("productId") final Long productId, @RequestParam(name = "uuid", required = false) final String uuid)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Product :{},  and uuid :{}", productId, uuid);
		final ProductResponseDTO productResponseDTO = productService.getProduct(productId, uuid);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.detail.message", null))
				.setData(productResponseDTO).create();
	}

	/**
	 * Get Cuisine with count
	 *
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/category/count/{vendorId}")
	public ResponseEntity<Object> getCategoryWiseProductCountList(@PathVariable final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside get category wise product count list");
		List<CategoryWiseProductCountDTO> cuisineWiseProductCountDTOList = productService.getCuisineWiseProductCountList(vendorId);
		LOGGER.info("Outside get category wise product count list");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("category.product.list.message", null)).setData(cuisineWiseProductCountDTOList).create();
	}

	/**
	 * @param  vendorId
	 * @param  cuisineId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/list/vendor/{vendorId}/cuisine/{cuisineId}")
	public ResponseEntity<Object> getProductListForCuisine(@PathVariable final Long vendorId, @PathVariable final Long cuisineId)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get cuise wise product list");
		List<ProductResponseDTO> productList = productService.getProductListForVendorAndCuisine(vendorId, cuisineId);
		LOGGER.info("Outside get cuisine wise product list");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("cuisine.product.list.message", null)).setData(productList).create();
	}

	/**
	 * Get product list based on parameters.This method will be used by all: Customer, Vendor and Admin and respective products will be shown to them.
	 *
	 * @param  accessToken
	 * @param  productParamRequestDTO
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/list/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getProductListBasedOnParams(@RequestBody final ProductParamRequestDTO productParamRequestDTO,
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize) throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Product List BasedOnParams {}", productParamRequestDTO);
		Long totalCount = productService.getProductCountBasedOnParams(productParamRequestDTO);
		PaginationUtilDto paginationUtilDto = PaginationUtil.calculatePagination(pageNumber, pageSize, totalCount);
		final List<ProductResponseDTO> productList = productService.getProductListBasedOnParams(productParamRequestDTO, paginationUtilDto.getStartIndex(),
				pageSize);
		LOGGER.info("After successfully Get Product List BasedOnParams");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_LIST_MESSAGE, null))
				.setData(productList).setHasNextPage(paginationUtilDto.getHasNextPage()).setHasPreviousPage(paginationUtilDto.getHasPreviousPage())
				.setTotalPages(paginationUtilDto.getTotalPages().intValue()).setPageNumber(paginationUtilDto.getPageNumber()).setTotalCount(totalCount)
				.create();
	}

	/**
	 * Get product list based on parameters.This method will be used by Customer only as for admin and vendor there is not need to group the products based on
	 * category
	 *
	 * @param  accessToken
	 * @param  productParamRequestDTO
	 * @param  pageNumber
	 * @param  pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/list/group/category")
	public ResponseEntity<Object> getProductListBasedOnParamsGroupedByCategory(@RequestBody final ProductParamRequestDTO productParamRequestDTO)
			throws NotFoundException, ValidationException {
		LOGGER.info("Inside get Product List BasedOnParams {}", productParamRequestDTO);
		final List<CategoryWiseProductReponseDto> productList = productService.getProductListBasedOnParamsAndCategoryWise(productParamRequestDTO);
		LOGGER.info("After successfully Get Product List BasedOnParams");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_LIST_MESSAGE, null))
				.setData(productList).create();
	}

	/**
	 * Change status of product
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  productId
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */

	@PutMapping("/status/{productId}")
	@PreAuthorize("hasPermission('Product List','CAN_DELETE')")
	public ResponseEntity<Object> changeStatus(@RequestHeader("Authorization") final String accessToken, @PathVariable("productId") final Long productId,
			@RequestParam("active") final Boolean active) throws NotFoundException, ValidationException {
		LOGGER.info("Inside change status of product for id {} and new status {}", productId, active);
		productService.changeStatus(productId, active);
		LOGGER.info("Outside change status of product ");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("product.update.message", null))
				.create();
	}

	/**
	 * export made product list
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  httpServletResponse
	 * @param  activeRecords
	 * @return
	 * @throws IOException
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileNotFoundException
	 */
	@PostMapping(value = "/export/list", produces = "text/csv")
	public ResponseEntity<Object> exportProductList(@RequestHeader("Authorization") final String accessToken,
			@RequestBody final ProductParamRequestDTO productParamRequestDTO, final HttpServletResponse httpServletResponse)
			throws ValidationException, NotFoundException, FileNotFoundException {
		productService.exportProductList(httpServletResponse, productParamRequestDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_LIST_MESSAGE, null))
				.create();
	}

	/**
	 * Upload product
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  file
	 * @param  httpServletResponse
	 * @return
	 * @throws BaseException
	 */
	@PostMapping(path = "/upload")
	@PreAuthorize("hasPermission('Product','CAN_ADD')")
	public ResponseEntity<Object> importData(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "file", required = true) final MultipartFile file, final HttpServletResponse httpServletResponse) throws BaseException {
		if (file == null) {
			throw new ValidationException(messageByLocaleService.getMessage("file.not.null", null));
		}
		productService.uploadFile(file, httpServletResponse);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_CREATE_MESSAGE, null))
				.create();
	}

	/**
	 * to delete image by type
	 *
	 * @param  accessToken
	 * @param  imageType
	 * @param  productId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@DeleteMapping("/{productId}")
	@PreAuthorize("hasPermission('Product','CAN_EDIT')")
	public ResponseEntity<Object> deleteImage(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "imageType") final String imageType, @PathVariable("productId") final Long productId)
			throws ValidationException, NotFoundException {
		productService.deleteImage(imageType, productId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage(PRODUCT_CREATE_MESSAGE, null))
				.create();
	}

}
