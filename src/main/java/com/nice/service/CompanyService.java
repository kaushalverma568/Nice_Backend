package com.nice.service;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CompanyDTO;
import com.nice.dto.CompanyResponseDTO;
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
	 */
	void addCompany(CompanyDTO company, final MultipartFile logo) throws ValidationException, NotFoundException;

	/**
	 * Updating Company and returning the updated object
	 *
	 * @param  company
	 * @param  logo
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateCompany(CompanyDTO company, final MultipartFile logo) throws NotFoundException, ValidationException;

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
