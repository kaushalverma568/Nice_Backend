package com.nice.service;

import java.util.List;
import java.util.Optional;

import com.nice.dto.ProductVariantRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.UOM;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
public interface ProductVariantService {

	/**
	 * @param product
	 * @return
	 */
	List<ProductVariant> getProductVariantByProduct(Product product, Boolean active);

	/**
	 * @param variantId
	 * @return
	 * @throws NotFoundException
	 */
	ProductVariant getProductVariantDetail(Long variantId) throws NotFoundException;

	/**
	 * @param product
	 * @param uom
	 * @return
	 */
	Optional<ProductVariant> getProductVariantDetailByProductAndUOMOptional(Product product, UOM uom);

	/**
	 * @param product
	 * @param uom
	 * @return
	 * @throws NotFoundException
	 */
	ProductVariant getProductVariantDetailByProductAndUOM(Product product, UOM uom) throws NotFoundException;

	/**
	 * active deactive product variant
	 *
	 * @param productVariantId
	 * @param active
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long productVariantId, Boolean active, Long userId) throws NotFoundException, ValidationException;

	/**
	 * @param productId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<ProductVariantResponseDTO> getProductVariantProductList(Long productId, Boolean active, Long pincodeId, Long storeId, Boolean isAdmin)
			throws NotFoundException, ValidationException;

	/**
	 * add/update list of product variant
	 *
	 * @param productId
	 * @param productVariantRequestDTOList
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateProductVariantList(Long productId, List<ProductVariantRequestDTO> productVariantRequestDTOList, Long userId)
			throws NotFoundException, ValidationException;

	/**
	 * @param productVariantList
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<ProductVariantResponseDTO> convertToResponseDtoList(List<ProductVariant> productVariantList, Long pincodeId)
			throws NotFoundException, ValidationException;

	/**
	 * @param productVariantId
	 * @param pincodeId
	 * @param storeId
	 * @param isAdmin
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductVariantResponseDTO getProductVariant(Long productVariantId, Long pincodeId, Long storeId, Boolean isAdmin)
			throws NotFoundException, ValidationException;

	/**
	 * @param sku
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	ProductVariantResponseDTO getProductVariantBySku(String sku, Long userId) throws NotFoundException, ValidationException;

	/**
	 * @param sku
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	ProductVariant getProductVariantDetailBySku(String sku, Long vendorId) throws NotFoundException;

	/**
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ProductVariantResponseDTO getProductVariant(Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 * @param product
	 * @param active
	 * @param pincodeId
	 * @param storeId
	 * @param isAdmin
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductVariantResponseDTO> getProductVariantDetailByProduct(Product product, Boolean active, Long pincodeId, Long storeId, Boolean isAdmin)
			throws NotFoundException, ValidationException;
}
