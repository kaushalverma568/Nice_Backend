package com.nice.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.ToppingDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Topping;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 14-07-2020
 */
public interface ToppingService {

	/**
	 * persist topping object
	 *
	 * @param toppingDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */

	void addTopping(ToppingDTO toppingDTO) throws ValidationException, NotFoundException;

	/**
	 * update topping
	 *
	 * @param toppingDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateTopping(ToppingDTO toppingDTO) throws NotFoundException, ValidationException;

	/**
	 * get DTO object of topping
	 *
	 * @param toppingId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ToppingDTO getTopping(Long toppingId) throws NotFoundException, ValidationException;

	/**
	 * change status of topping (active/deActive)
	 *
	 * @param toppingId
	 * @param isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long toppingId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check topping duplication and returning Boolean value.
	 *
	 * @param toppingDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isToppingExists(ToppingDTO toppingDTO);

	/**
	 * get detail object of topping
	 *
	 * @param toppingId
	 * @return
	 * @throws NotFoundException
	 */
	Topping getToppingDetail(Long toppingId) throws NotFoundException;

	/**
	 * get list of Toppings
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword
	 * @return
	 * @throws NotFoundException
	 */
	Page<Topping> getToppingList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword);

	/**
	 * 
	 * @param file
	 * @param httpServletResponse
	 * @throws FileOperationException 
	 */
	void uploadFile(MultipartFile file, HttpServletResponse httpServletResponse) throws FileOperationException;
}
