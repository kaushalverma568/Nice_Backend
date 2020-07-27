package com.nice.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtrasMaster;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface ProductExtrasMasterService {

	/**
	 * @param  productExtrasMasterDto
	 * @return
	 */
	boolean isExists(ProductExtrasMasterDTO productExtrasMasterDto);

	/**
	 * @param  productExtrasMasterDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long addProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDto) throws NotFoundException, ValidationException;

	/**
	 * @param  productExtrasMasterDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long updateProductExtrasMaster(ProductExtrasMasterDTO productExtrasMasterDTO) throws NotFoundException, ValidationException;

	/**
	 * @param  productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtrasMasterDTO getProductExtrasMaster(Long productExtrasMasterId) throws NotFoundException;

	/**
	 * @param  productExtrasMasterId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long productExtrasMasterId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtrasMaster getProductExtrasMasterDetail(Long productExtrasMasterId) throws NotFoundException;

	/**
	 * @param productExtrasMasterId
	 */
	void deleteProductExtrasMaster(Long productExtrasMasterId);

	/**
	 * @param pageSize 
	 * @param pageNumber 
	 * @param  activeRecords
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	Page<ProductExtrasMaster> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Long vendorId) throws NotFoundException;

	/**
	 * @param  file
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

}
