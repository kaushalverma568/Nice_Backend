package com.nice.service;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.CuisineDTO;
import com.nice.dto.CuisineResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Cuisine;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
public interface CuisineService {
	/**
	 * Add cuisine
	 *
	 * @param  cuisineDTO
	 * @param  image
	 * @param  userId
	 * @return
	 * @throws FileOperationException
	 * @throws ValidationException
	 */
	void addCuisine(CuisineDTO cuisineDTO, MultipartFile image) throws FileOperationException, ValidationException;

	/**
	 * Update cuisine
	 *
	 * @param  image
	 * @param  cuisine
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileOperationException
	 */
	void updateCuisine(CuisineDTO cuisineDTO, MultipartFile image) throws NotFoundException, ValidationException, FileOperationException;

	/**
	 * Get details of cuisine
	 *
	 * @param  cuisineId
	 * @return
	 * @throws NotFoundException
	 */
	CuisineResponseDTO getCuisine(Long cuisineId) throws NotFoundException;

	/**
	 * Change status of cuisine (active/deActive)
	 *
	 * @param  cuisineId
	 * @param  active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long cuisineId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * to check cuisine duplication and returning Boolean value.
	 *
	 * @param  cuisine
	 * @return
	 * @throws ValidationException
	 */
	boolean isCuisineExistsEnglish(CuisineDTO cuisineDTO);

	/**
	 * to check cuisine duplication and returning Boolean value.
	 *
	 * @param  cuisine
	 * @return
	 * @throws ValidationException
	 */
	boolean isCuisineExistsArabic(CuisineDTO cuisineDTO);

	/**
	 * Get List of cuisine based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<Cuisine> getCuisineList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord)
			throws NotFoundException, ValidationException;

	/**
	 * Get Cuisine details based on Id : Specially for internally calls
	 *
	 * @param  cuisineId
	 * @return
	 * @throws NotFoundException
	 */
	Cuisine getCuisineDetails(Long cuisineId) throws NotFoundException;

	/**
	 * @param  cuisineId
	 * @throws NotFoundException
	 */
	void deleteImage(Long cuisineId) throws NotFoundException;
}
