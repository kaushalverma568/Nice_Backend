package com.nice.service;

import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtrasMaster;

public interface ProductExtrasMasterService {

	/**
	 * 
	 * @param productExtrasMasterDto
	 * @return
	 */
	boolean isExists(ProductExtrasMasterDTO productExtrasMasterDto);

	/**
	 * 
	 * @param productExtrasMasterDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long addProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDto) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @param productExtrasMasterDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long updateProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDTO) throws NotFoundException, ValidationException;

	/**
	 * 
	 * @param productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtrasMasterDTO getProductExtrasMaster(Long productExtrasMasterId) throws NotFoundException;

	/**
	 * 
	 * @param productExtrasMasterId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long productExtrasMasterId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtrasMaster getProductExtrasMasterDetail(Long productExtrasMasterId) throws NotFoundException;

	/**
	 * 
	 * @param productExtrasMasterId
	 */
	void deleteProductExtrasMaster(Long productExtrasMasterId);

	/**
	 * 
	 * @param activeRecords
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtrasMasterDTO> getList(Boolean activeRecords, Long vendorId) throws NotFoundException;

	/**
	 * 
	 * @param file
	 * @param httpServletResponse
	 * @throws FileOperationException 
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

}
