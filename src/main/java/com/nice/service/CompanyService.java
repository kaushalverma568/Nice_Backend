package com.nice.service;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CompanyDTO;
import com.nice.dto.CompanyResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Company;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface CompanyService {

	/**
	 * Adding Company and getting the added object in return
	 *
	 * @param  company
	 * @param  logo
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileOperationException
	 */
	void addCompany(CompanyDTO company, final MultipartFile logo) throws ValidationException, NotFoundException, FileOperationException;

	/**
	 * Updating Company and returning the updated object
	 *
	 * @param  company
	 * @param  logo
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void updateCompany(CompanyDTO company, final MultipartFile logo) throws NotFoundException, ValidationException, FileOperationException;

	/**
	 * Get company
	 *
	 * @param  isImageRequired
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	CompanyResponseDTO getCompany(Boolean isImageRequired) throws NotFoundException;

	/**
	 * @param  companyId
	 * @return
	 * @throws NotFoundException
	 */
	Company getCompanyDetail(Long companyId) throws NotFoundException;
}
