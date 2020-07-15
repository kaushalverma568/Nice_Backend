package com.nice.service;

import java.util.List;

import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtras;

public interface ProductExtrasService {

	boolean isExists(ProductExtrasDTO productExtrasDto) throws NotFoundException;

	Long addProductExtras(ProductExtrasDTO productExtrasDto) throws NotFoundException, ValidationException;

	Long updateProductExtras(ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException;

	ProductExtrasDTO getProductExtras(Long productExtrasId) throws NotFoundException;

	void changeStatus(Long productExtrasId, Boolean active) throws ValidationException, NotFoundException;

	ProductExtras getProductExtrasDetail(Long productExtrasId) throws NotFoundException;

	void deleteProductExtras(Long productExtrasId);

	/**
	 * @param productId
	 * @param activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductExtrasDTO> getListWithUserCheck(Long productId, Boolean activeRecords) throws NotFoundException, ValidationException;

	/**
	 * @param activeRecords
	 * @param productId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtrasDTO> getList(Boolean activeRecords, Long productId) throws NotFoundException;

	/**
	 * 
	 * @param activeRecords
	 * @param productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtras> getListByProductExtrasMaster(Boolean activeRecords, Long productExtrasMasterId)
			throws NotFoundException;

}
