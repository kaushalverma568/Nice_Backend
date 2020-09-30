package com.nice.service;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.nice.dto.DiscountDTO;
import com.nice.dto.DiscountResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Discount;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 27-Mar-2020
 * @description :
 */
public interface DiscountService {

	/**
	 * Add discount
	 *
	 * @param discountDTO
	 * @param userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addDiscount(DiscountDTO discountDTO) throws ValidationException, NotFoundException;

	/**
	 * Get discount details based on discountId
	 *
	 * @param discountId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	DiscountResponseDTO getDiscount(final Long discountId) throws NotFoundException, ValidationException;

	/**
	 * Get Discount details based on Id : Specially for internally calls
	 *
	 * @param discountId
	 * @return
	 * @throws NotFoundException
	 */
	Discount getDiscountDetails(Long discountId) throws NotFoundException;

	/**
	 * Change status of discount
	 *
	 * @param discountId
	 * @param status
	 * @param userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long discountId, String status) throws ValidationException, NotFoundException;

	/**
	 * update discount
	 *
	 * @param discountDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateDiscount(DiscountDTO discountDTO) throws ValidationException, NotFoundException;

	/**
	 * Get discount list based on parameters
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param status
	 * @param vendorId
	 * @return
	 */
	Page<Discount> getDiscountListBasedOnParams(Integer pageNumber, Integer pageSize, String status, Long vendorId);

	/**
	 * get discount list in response dto
	 *
	 * @param discounts
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<DiscountResponseDTO> getDiscountListBasedOnParams(List<Discount> discounts) throws NotFoundException, ValidationException;

	/**
	 * activate or expire discount according to start date and end date of discount
	 *
	 * @param runDate
	 */
	void activateExpireDiscount(LocalDate runDate);

	/**
	 * @param discountId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Map<String, String> getProductListOfThatDiscount(Long discountId) throws NotFoundException, ValidationException;

}
