package com.nice.service;

import java.util.List;

import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtras;

public interface ProductExtrasService {

	boolean isExists(ProductExtrasDTO productExtrasDto) throws ValidationException;

	ProductExtrasDTO addProductExtras(ProductExtrasDTO productExtrasDto) throws NotFoundException;

	ProductExtrasDTO updateProductExtras(ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException;

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
	List<ProductExtrasDTO> getList(Long productId, Boolean activeRecords) throws NotFoundException, ValidationException;

}
