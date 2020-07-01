package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.OrderItemRatingDTO;
import com.nice.dto.OrderItemRatingResponseDTO;
import com.nice.model.OrderItemRating;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

public interface OrderItemRatingService {

	/**
	 * 
	 * @param orderItemRatingDto
	 * @return
	 * @throws NotFoundException
	 */
	public OrderItemRatingResponseDTO addOrderItemRating( OrderItemRatingDTO orderItemRatingDto) throws NotFoundException ;

	/**
	 * 
	 * @param orderItemRatingId
	 * @return
	 * @throws NotFoundException
	 */
	public OrderItemRatingResponseDTO getOrderItemRating(Long orderItemRatingId) throws NotFoundException;

	/**
	 * 
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @return
	 */
	public Page<OrderItemRating> getList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * 
	 * @param orderItemRatingId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	public void changeStatus(Long orderItemRatingId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param orderItemRatingId
	 * @return
	 * @throws NotFoundException
	 */
	OrderItemRating getOrderItemRatingDetail(Long orderItemRatingId) throws NotFoundException;

	/**
	 * 
	 * @param productId
	 * @return
	 */
	List<OrderItemRating> getOrderRatingByProductId(Long productId);

	/**
	 * 
	 * @param orderRatingId
	 * @return
	 */
	List<OrderItemRating> getOrderRatingByOrderRatingId(Long orderRatingId);
	
}
