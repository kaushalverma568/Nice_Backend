package com.nice.service;

import java.io.IOException;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.UOMDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.UOM;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
public interface UOMService {

	/**
	 * persist uom object
	 *
	 * @param uomDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addUOM(UOMDTO uomDTO) throws ValidationException, NotFoundException;

	/**
	 * update uom
	 *
	 * @param uomDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateUOM(UOMDTO uomDTO) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of uom
	 *
	 * @param uomId
	 * @return
	 * @throws NotFoundException
	 */
	UOMDTO getUOM(Long uomId) throws NotFoundException;

	/**
	 * change status of uom (active/deActive)
	 *
	 * @param uomId
	 * @param isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long uomId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check uom duplication and returning Boolean value.
	 *
	 * @param uomDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isUOMExists(UOMDTO uomDTO);

	/**
	 * get detail object of uom
	 *
	 * @param uomId
	 * @return
	 * @throws NotFoundException
	 */
	UOM getUOMDetail(Long uomId) throws NotFoundException;

	/**
	 * get list of uoms
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param vendorId 
	 * @return
	 * @throws NotFoundException
	 */
	Page<UOM> getUOMList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Long vendorId) throws NotFoundException;

	/**
	 * 
	 * @param activeRecords
	 * @param httpServletResponse
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	void exportList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws  FileNotFoundException;

	/**
	 * 
	 * @param file
	 * @param httpServletResponse
	 * @throws FileOperationException 
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

}
