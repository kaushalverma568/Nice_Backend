package com.nice.service;

import java.util.List;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtrasMaster;

public interface ProductExtrasMasterService {

	boolean isExists(ProductExtrasMasterDTO productExtrasMasterDto);

	Long addProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDto) throws NotFoundException, ValidationException;

	Long updateProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDTO) throws NotFoundException, ValidationException;

	ProductExtrasMasterDTO getProductExtrasMaster(Long productExtrasMasterId) throws NotFoundException;

	void changeStatus(Long productExtrasMasterId, Boolean active) throws ValidationException, NotFoundException;

	ProductExtrasMaster getProductExtrasMasterDetail(Long productExtrasMasterId) throws NotFoundException;

	void deleteProductExtrasMaster(Long productExtrasMasterId);

	List<ProductExtrasMasterDTO> getList(Boolean activeRecords, Long vendorId) throws NotFoundException;

}
