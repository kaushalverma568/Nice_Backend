package com.nice.service;

import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ProductAttributeDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductAttribute;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface ProductAttributeService {

	boolean isExists(ProductAttributeDTO productAttributeDto) throws ValidationException;

	ProductAttributeDTO addProductAttribute(ProductAttributeDTO productAttributeDto) throws NotFoundException, ValidationException;

	ProductAttributeDTO updateProductAttribute(ProductAttributeDTO productAttributeDTO) throws NotFoundException, ValidationException;

	ProductAttributeDTO getProductAttribute(Long productAttributeId) throws NotFoundException, ValidationException;

	void changeStatus(Long productAttributeId, Boolean active) throws ValidationException, NotFoundException;

	ProductAttribute getProductAttributeDetail(Long productAttributeId) throws NotFoundException;

	void deleteProductAttribute(Long productAttributeId);

	/**
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param vendorId TODO
	 * @return
	 * @throws ValidationException
	 */
	Page<ProductAttribute> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Long vendorId) throws ValidationException;

	/**
	 * @return
	 * @throws ValidationException
	 */
	List<ProductAttribute> getAllActiveList() throws ValidationException;

	/**
	 * @param  file
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

}
