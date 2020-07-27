package com.nice.service;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CategoryWiseProductCountDTO;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.dto.ProductResponseDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
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
	 * @throws ValidationException
	 */
	boolean isProductExists(ProductRequestDTO productRequestDTO) throws ValidationException;

	/**
	 * add product
	 *
	 * @param productRequestDTO
	 * @param userId
	 * @param image
	 * @param detailImage 
	 * @param thumbnailImage
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addProduct(ProductRequestDTO productRequestDTO, MultipartFile image, MultipartFile detailImage) throws NotFoundException, ValidationException;

	/**
	 * get product responseDTO by id
	 *
	 * @param productId
	 * @param uuid
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductResponseDTO getProduct(Long productId, String uuid) throws NotFoundException, ValidationException;

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
	 * @param image
	 * @param detailImage 
	 * @param thumbnailImage
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateProduct(ProductRequestDTO productRequestDTO, MultipartFile image, MultipartFile detailImage) throws NotFoundException, ValidationException;

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
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long productId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * get product list based on parameters
	 *
	 * @param productParamRequestDTO
	 * @param startIndex
	 * @param pageSize
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductResponseDTO> getProductListBasedOnParams(ProductParamRequestDTO productParamRequestDTO, Integer startIndex, Integer pageSize)
			throws NotFoundException, ValidationException;

	/**
	 * get product count based on parameters
	 *
	 * @param productParamRequestDTO
	 * @return
	 */
	Long getProductCountBasedOnParams(ProductParamRequestDTO productParamRequestDTO);

	List<Product> getProductListBasedOnParamsWithoutPagination(ProductParamRequestDTO productParamRequestDTO);

	/**
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	List<CategoryWiseProductCountDTO> getCuisineWiseProductCountList(Long vendorId) throws NotFoundException;

	/**
	 * @param vendorId
	 * @param cuisineId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductResponseDTO> getProductListForVendorAndCategory(Long vendorId, Long cuisineId) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param productId
	 * @param newRating
	 * @throws NotFoundException
	 */
	void updateProductRating(Long productId, Double newRating) throws NotFoundException;

	/**
	 * @param vendorId
	 * @param cuisineId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductResponseDTO> getProductListForVendorAndCuisine(Long vendorId, Long cuisineId) throws NotFoundException, ValidationException;

	/**
	 * export by filter dto
	 *
	 * @param httpServletResponse
	 * @param productParamRequestDTO
	 * @throws IOException
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileNotFoundException 
	 */
	void exportProductList(HttpServletResponse httpServletResponse, ProductParamRequestDTO productParamRequestDTO)
			throws  NotFoundException, ValidationException, FileNotFoundException;

	/**
	 * @param multipartFile
	 * @param httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile multipartFile, HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 *  to delete image
	 * @param imageType
	 * @param productId
	 * @throws NotFoundException 
	 * @throws ValidationException 
	 */
	void deleteImage(String imageType, Long productId) throws NotFoundException, ValidationException;

	/**
	 * get response of global search
	 *
	 * @param searchKeyword
	 * @return
	 */
	// GlobalSearchResponseDTO getResultOfGlobalSearch(String searchKeyword);

}
