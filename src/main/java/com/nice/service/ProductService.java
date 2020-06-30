package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Product;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
public interface ProductService {

	/**
	 * product ith same name exist or not
	 *
	 * @param productRequestDTO
	 * @return
	 */
	boolean isProductExists(ProductRequestDTO productRequestDTO);

	/**
	 * add product
	 *
	 * @param productRequestDTO
	 * @param userId
	 * @param image
	 * @param thumbnailImage
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addProduct(ProductRequestDTO productRequestDTO, Long userId, MultipartFile image) throws NotFoundException, ValidationException;

	/**
	 * get product responseDTO by id
	 *
	 * @param productId
	 * @param isAdmin
	 * @param uuid
	 * @param customerId
	 * @param pincodeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductResponseDTO getProduct(Long productId, Long customerId, String uuid, Boolean isAdmin, Long pincodeId) throws NotFoundException, ValidationException;

	/**
	 * get product by id
	 *
	 * @param productId
	 * @return
	 * @throws NotFoundException
	 */
	Product getProductDetail(Long productId) throws NotFoundException;

	/**
	 * update product
	 *
	 * @param productRequestDTO
	 * @param userId
	 * @param image
	 * @param thumbnailImage
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateProduct(ProductRequestDTO productRequestDTO, Long userId, MultipartFile image) throws NotFoundException, ValidationException;

	/**
	 * get product page
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @return
	 */
	Page<Product> getProductList(Integer pageNumber, Integer pageSize);

	/**
	 * get product detail list
	 *
	 * @param products
	 * @param pincodeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductResponseDTO> getProductDetailList(List<Product> products) throws NotFoundException, ValidationException;

	/**
	 * @param productId
	 * @param active
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long productId, Boolean active, Long userId) throws NotFoundException, ValidationException;

	/**
	 * get product list based on parameters
	 *
	 * @param productParamRequestDTO
	 * @param startIndex
	 * @param pageSize
	 * @param pincodeId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductResponseDTO> getProductListBasedOnParams(ProductParamRequestDTO productParamRequestDTO, Integer startIndex, Integer pageSize,
			Boolean listForAdmin, Long pincodeId) throws NotFoundException, ValidationException;

	/**
	 * get product count based on parameters
	 *
	 * @param productParamRequestDTO
	 * @return
	 */
	Long getProductCountBasedOnParams(ProductParamRequestDTO productParamRequestDTO);

	List<Product> getProductListBasedOnParamsWithoutPagination(ProductParamRequestDTO productParamRequestDTO);

	/**
	 * @param pageNumber
	 * @param pageSize
	 * @param vendorId
	 * @return
	 */
	Page<Product> getProductListForVendor(Integer pageNumber, Integer pageSize, Long vendorId);

	/**
	 * get response of global search
	 *
	 * @param searchKeyword
	 * @return
	 */
	// GlobalSearchResponseDTO getResultOfGlobalSearch(String searchKeyword);

	/**
	 * get category wise product count list
	 *
	 * @return
	 */
	// List<CategoryWiseProductCountDTO> getCategoryWiseProductCountList();

}
