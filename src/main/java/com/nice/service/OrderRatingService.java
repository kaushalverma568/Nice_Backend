package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.OrderRating;

public interface OrderRatingService {

	/**
	 * 
	 * @param orderRatingDto
	 * @return
	 */
	public boolean isExists(OrderRatingDTO orderRatingDto) ;

	/**
	 * 
	 * @param orderRatingDto
	 * @return
	 * @throws NotFoundException
	 */
	public OrderRatingResponseDTO addOrderRating( OrderRatingDTO orderRatingDto) throws NotFoundException ;

	/**
	 * 
	 * @param orderRatingId
	 * @return
	 * @throws NotFoundException
	 */
	public OrderRatingResponseDTO getOrderRating(Long orderRatingId) throws NotFoundException;

	/**
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param searchKeyWord
	 * @return
	 */
	public Page<OrderRating> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord);

	/**
	 * 
	 * @param orderRatingId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void changeStatus(Long orderRatingId, Boolean active) throws ValidationException, NotFoundException;
 
	/**
	 * 
	 * @param orderRatingId
	 * @return
	 * @throws NotFoundException
	 */
	OrderRating getOrderRatingDetail(Long orderRatingId) throws NotFoundException;

	/**
	 * 
	 * @param deliveryBoyId
	 * @return
	 */
	List<OrderRating> getOrderRatingByDeliveryBoyId(Long deliveryBoyId);

	/**
	 * 
	 * @param vendorId
	 * @return
	 */
	List<OrderRating> getOrderRatingByVendorId(Long vendorId);

	/**
	 * @throws NotFoundException 
	 * 
	 */
	void calculateRating() throws NotFoundException;

	/**
	 * 
	 * @param orderId
	 * @return
	 * @throws NotFoundException
	 */
	public OrderRatingResponseDTO getOrderRatingbyOrderId(Long orderId) throws NotFoundException;
	
}
