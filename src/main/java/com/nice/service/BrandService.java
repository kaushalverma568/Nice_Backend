package com.nice.service;

import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.BrandDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Brand;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
public interface BrandService {

	/**
	 * persist brand object
	 *
	 * @param brandDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addBrand(BrandDTO brandDTO) throws NotFoundException;

	/**
	 * update brand
	 *
	 * @param brandDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateBrand(BrandDTO brandDTO) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of brand
	 *
	 * @param brandId
	 * @return
	 * @throws NotFoundException
	 */
	BrandDTO getBrand(Long brandId) throws NotFoundException;

	/**
	 * change status of brand (active/deActive)
	 *
	 * @param brandId
	 * @param isActive
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long brandId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check brand duplication and returning Boolean value.
	 *
	 * @param brandDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isBrandExists(BrandDTO brandDTO);

	/**
	 * get detail object of brand
	 *
	 * @param brandId
	 * @return
	 * @throws NotFoundException
	 */
	Brand getBrandDetail(Long brandId) throws NotFoundException;

	/**
	 * get list of brands
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyword
	 * @return
	 * @throws NotFoundException
	 */
	Page<Brand> getBrandList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword) throws NotFoundException;

	/**
	 * export of brands
	 *
	 * @param httpServletResponse
	 * @throws FileOperationException
	 */
	void exportBrandList(HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 * import brands
	 *
	 * @param file
	 * @param userId
	 * @param httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 * get Brand list by searchKeyword and active
	 *
	 * @param active
	 * @param searchKeyword
	 * @return
	 */
	List<Brand> getBrandList(Boolean active, String searchKeyword);
}
