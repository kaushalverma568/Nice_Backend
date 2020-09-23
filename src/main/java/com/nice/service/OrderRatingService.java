package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrderRating;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface OrderRatingService {

	/**
	 * @param  orderRatingDto
	 * @return
	 */
	boolean isExists(OrderRatingDTO orderRatingDto);

	/**
	 * @param  orderRatingDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException 
	 */
	OrderRatingResponseDTO addOrderRating(OrderRatingDTO orderRatingDto) throws NotFoundException, ValidationException;

	/**
	 * @param  orderRatingId
	 * @return
	 * @throws NotFoundException
	 */
	OrderRatingResponseDTO getOrderRating(Long orderRatingId) throws NotFoundException;

	/**
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @return
	 */
	Page<OrderRating> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord);

	/**
	 * @param  orderRatingId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long orderRatingId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  orderRatingId
	 * @return
	 * @throws NotFoundException
	 */
	OrderRating getOrderRatingDetail(Long orderRatingId) throws NotFoundException;

	/**
	 * @param pageSize 
	 * @param pageNumber 
	 * @param  deliveryBoyId
	 * @return
	 */
	Page<OrderRating> getOrderRatingByDeliveryBoyId(Integer pageNumber, Integer pageSize, Long deliveryBoyId);

	/**
	 * @param pageSize 
	 * @param pageNumber 
	 * @param  vendorId
	 * @return
	 */
	Page<OrderRating> getOrderRatingByVendorId(Integer pageNumber, Integer pageSize, Long vendorId);

	/**
	 * @throws NotFoundException
	 */
	void calculateRating() throws NotFoundException;

	/**
	 * @param  orderId
	 * @return
	 * @throws NotFoundException
	 */
	OrderRatingResponseDTO getOrderRatingbyOrderId(Long orderId) throws NotFoundException;

}
