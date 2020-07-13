package com.nice.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CategoryDTO;
import com.nice.dto.CategoryResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Category;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
public interface CategoryService {

	/**
	 * persist category object
	 *
	 * @param categoryDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addCategory(CategoryDTO categoryDTO, final MultipartFile image) throws ValidationException, NotFoundException;

	/**
	 * update category
	 *
	 * @param categoryDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateCategory(CategoryDTO categoryDTO, final MultipartFile image) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of category
	 *
	 * @param categoryId
	 * @return
	 * @throws NotFoundException
	 */
	CategoryResponseDTO getCategory(Long categoryId) throws NotFoundException;

	/**
	 * change status of category (active/deActive)
	 *
	 * @param categoryId
	 * @param isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long categoryId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check category duplication and returning Boolean value.
	 *
	 * @param categoryDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isCategoryExists(CategoryDTO categoryDTO) throws NotFoundException;

	/**
	 * get detail object of category
	 *
	 * @param categoryId
	 * @return
	 * @throws NotFoundException
	 */
	Category getCategoryDetail(Long categoryId) throws NotFoundException;

	/**
	 * get list of categories
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword
	 * @return
	 * @throws NotFoundException
	 */
	Page<Category> getCategoryList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword, final Long vendorId)
			throws NotFoundException;

	/**
	 * export of category
	 *
	 * @param httpServletResponse
	 * @throws FileOperationException
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void exportCategoryList(HttpServletResponse httpServletResponse) throws FileOperationException, ValidationException, NotFoundException;

	/**
	 * upload file
	 *
	 * @param file
	 * @param httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;
}
