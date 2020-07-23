package com.nice.service;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.SubCategoryDTO;
import com.nice.dto.SubCategoryResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Category;
import com.nice.model.SubCategory;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
public interface SubCategoryService {
	/**
	 * persist sub category object
	 *
	 * @param  subCategoryDTO
	 * @param  image
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addSubCategory(SubCategoryDTO subCategoryDTO, MultipartFile image) throws ValidationException, NotFoundException;

	/**
	 * update sub category
	 *
	 * @param  subCategoryDTO
	 * @param  image
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateSubCategory(SubCategoryDTO subCategoryDTO, MultipartFile image) throws ValidationException, NotFoundException;

	/**
	 * get DTO object of sub category
	 *
	 * @param  subCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	SubCategoryResponseDTO getSubCategory(final Long subCategoryId) throws NotFoundException;

	/**
	 * get list of sub categories
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  allRecords
	 * @param  categoryId
	 * @return
	 * @throws NotFoundException
	 */
	Page<SubCategory> getSubCategoryList(final Integer pageNumber, final Integer pageSize, Boolean allRecords, Long categoryId) throws NotFoundException;

	/**
	 * change status of sub category (active/deActive)
	 *
	 * @param  subCategoryId
	 * @param  isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long subCategoryId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check sub category duplication and returning Boolean value.
	 *
	 * @param  subCategoryDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isSubCategoryExists(SubCategoryDTO subCategoryDTO) throws NotFoundException;

	/**
	 * get list of sub categories by category and active
	 *
	 * @param  category
	 * @param  active
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<SubCategory> getSubCategoryListByCategoryAndActive(Category category, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * get detail object of sub category
	 *
	 * @param  subCategoryId
	 * @return
	 * @throws NotFoundException
	 */
	SubCategory getSubCategoryDetail(Long subCategoryId) throws NotFoundException;

	/**
	 * get sub category list by category
	 *
	 * @param  category
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<SubCategory> getSubCategoryListByCategory(Category category) throws NotFoundException, ValidationException;

	/**
	 * export of sub category
	 *
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws IOException
	 */
	void exportSubCategoryList(HttpServletResponse httpServletResponse) throws FileOperationException, ValidationException, NotFoundException;

	/**
	 * upload file
	 *
	 * @param  file
	 * @param  httpServletResponse
	 * @throws FileOperationException
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;

	/**
	 * get sub category list by active and search keyword
	 *
	 * @param  active
	 * @param  searchKeyword
	 * @return
	 */
	List<SubCategory> getSubCategoryList(Boolean active, String searchKeyword);

	/**
	 * delete sub-category image
	 *
	 * @param  categoryId
	 * @throws NotFoundException
	 */
	void deleteImage(Long subCategoryId) throws NotFoundException;

}
