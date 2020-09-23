package com.nice.service.impl;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.DeliveryType;
import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderRatingMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.OrderRating;
import com.nice.model.Orders;
import com.nice.model.Vendor;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.repository.OrderRatingRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrderRatingService;
import com.nice.service.OrdersService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Dec-2019
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class OrderRatingServiceImpl implements OrderRatingService {

	private static final Logger LOGGER = LoggerFactory.getLogger(OrderRatingServiceImpl.class);
	
	@Autowired
	private OrderRatingRepository orderRatingRepository;

	@Autowired
	private OrderRatingMapper orderRatingMapper;

	@Autowired
	private VendorService vendorService;
	
	@Autowired
	private OrdersService orderService;
	
	@Autowired
	private VendorRepository vendorRepository;
		
	@Autowired
	private DeliveryBoyService deliveryBoyService;
	
	@Autowired
	private DeliveryBoyRepository deliveryBoyRepository;
	
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	

	@Override
	public OrderRatingResponseDTO addOrderRating(final OrderRatingDTO orderRatingDTO) throws NotFoundException, ValidationException {
	    validateOrderRating(orderRatingDTO);
		OrderRating orderRating= orderRatingMapper.toEntity(orderRatingDTO);
		Orders order = orderService.getOrderById(orderRating.getOrderId());
		orderRating.setVendorId(order.getVendor().getId());
		if (order.getDeliveryBoy() != null) {
			orderRating.setDeliveryBoyId(order.getDeliveryBoy().getId());
		}
	
		/**
		 *  first 3 rating for vendor 
		 * for vendor rating, we total question1 rating, question2 rating and question3 rating
		 * 		 divide by 3 because its total of 3 types of rating so we get average
		 */
		orderRating.setVendorRating(Math.round(((orderRating.getQuestion1Rating()
				+orderRating.getQuestion2Rating()+orderRating.getQuestion3Rating())/3.0)*100.0)/100.0);
		
		
		/**
		 *  4th and 5th rating is for delivery boy
		 * for delivery boy rating, we total question4 rating and question5 rating
		 * 		 divide by 2 because its total of 2 types of rating so we get average
		 */
		if (orderRating.getQuestion4Rating() != null && orderRating.getQuestion5Rating() != null) {
			orderRating.setDeliveryBoyRating(Math.round(((orderRating.getQuestion4Rating()
					+orderRating.getQuestion5Rating())/2.0)*100.0)/100.0);	
		} else {
			orderRating.setDeliveryBoyRating(0.0);
		}
		
		
		/**
		 *  over all average of order 
		 * for average rating, we total all 5 question rating
		 * 		 divide by 5 because its total of 5 types of rating so we get average
		 */
		if (orderRating.getQuestion4Rating() != null && orderRating.getQuestion5Rating() != null) {
		orderRating.setAvgOrderRating(Math.round(((orderRating.getQuestion1Rating()
				+orderRating.getQuestion2Rating()+orderRating.getQuestion3Rating()
				+orderRating.getQuestion4Rating()+orderRating.getQuestion5Rating())/5.0)*100.0)/100.0);
		}else {
			orderRating.setAvgOrderRating(Math.round(((orderRating.getQuestion1Rating()
					+orderRating.getQuestion2Rating()+orderRating.getQuestion3Rating())/3.0)*100.0)/100.0);
		}
		/**
		 * order rating save
		 */	
		return orderRatingMapper.toResponseDto(orderRatingRepository.save(orderRating));
	}

	
	private void validateOrderRating(OrderRatingDTO orderRatingDTO) throws NotFoundException, ValidationException {
		Orders order = orderService.getOrderById(orderRatingDTO.getOrderId());
		if (order.getDeliveryType().equalsIgnoreCase(DeliveryType.DELIVERY.name()) 
				|| order.getDeliveryType().equalsIgnoreCase(DeliveryType.BOTH.name())) {
			if (orderRatingDTO.getQuestion4Rating() == null) {
				throw new  ValidationException(messageByLocaleService.getMessage("question4.rating.not.null", null));
			}else if (orderRatingDTO.getQuestion5Rating() == null) {
				throw new  ValidationException(messageByLocaleService.getMessage("question5.rating.not.null", null));
			}
		}
	}


	@Override
	public OrderRatingResponseDTO getOrderRating(final Long orderRatingId) throws NotFoundException {
		calculateRating();
		return orderRatingMapper.toResponseDto(getOrderRatingDetail(orderRatingId));
	}

	@Override
	public void changeStatus(final Long orderRatingId, final Boolean active) throws ValidationException, NotFoundException {
		OrderRating existingOrderRating = getOrderRatingDetail(orderRatingId);
		LOGGER.info("Existing  OrderRating details {} ", existingOrderRating);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingOrderRating.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "order.rating.active" : "order.rating.deactive", null));
		} else {
			existingOrderRating.setActive(active);
			orderRatingRepository.save(existingOrderRating);
		}
	}

	@Override
	public Page<OrderRating> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords,String searchKeyWord) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC,"id"));
		if (activeRecords != null) {
			if (searchKeyWord != null) {
				return orderRatingRepository.findAllByActiveAndReviewContainingIgnoreCase(activeRecords, searchKeyWord, pageable);
			} else {
				return orderRatingRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyWord != null) {
				return orderRatingRepository.findAllByReviewContainingIgnoreCase(searchKeyWord, pageable);
			} else {
				return orderRatingRepository.findAll(pageable);
			}
		}
	}

	@Override
	public boolean isExists(final OrderRatingDTO orderRatingDTO) {
		if (orderRatingDTO.getId() != null) {
			return (orderRatingRepository.findByOrderIdAndIdNot(orderRatingDTO.getOrderId(), orderRatingDTO.getId()).isPresent());

		} else {
			return (orderRatingRepository.findByOrderId(orderRatingDTO.getOrderId()).isPresent());
		}
	}

	@Override
	public OrderRating getOrderRatingDetail(final Long orderRatingId) throws NotFoundException {
		return orderRatingRepository.findById(orderRatingId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("order.rating.not.found", new Object[] {  orderRatingId })));
	}
	
	@Override
	public  Page<OrderRating> getOrderRatingByDeliveryBoyId (final Integer pageNumber, final Integer pageSize, final Long deliveryBoyId){
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC,"id"));
		return  orderRatingRepository.findByDeliveryBoyId(deliveryBoyId,pageable);
	}

	@Override
	public  Page<OrderRating> getOrderRatingByVendorId (final Integer pageNumber, final Integer pageSize, final Long vendorId){
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by(Direction.DESC,"id"));
		return  orderRatingRepository.findByVendorId(vendorId,pageable);
	}
	
	public List<OrderRating> getOrderRatingByCreatedAt (final Date startDate, Date endDate){
		return  orderRatingRepository.findByCreatedAtbetween(startDate, endDate);
	}


	@Override
	public void calculateRating() throws NotFoundException {
		/**
		 * we are going to display rating till yesterday so every day at day ending schedular runs and calculate till today
		 * from yester day starting to today starting   
		 */
		LocalDate today = LocalDate.now();
		LocalDate yesterday = today.minusDays(1);
		Date startDate = Date.from(yesterday.atStartOfDay(ZoneId.systemDefault()).toInstant());
		Date endDate = Date.from(today.atStartOfDay(ZoneId.systemDefault()).toInstant());
		/**
		 * fetch all record which has createdAt Date between yesterday to today with starting time
		 */
		List<OrderRating> orderRatingList = getOrderRatingByCreatedAt(startDate , endDate);
		 for (OrderRating orderRating : orderRatingList) {
			 // Restaurant rating calculation
               vendorRatingVCalculation(orderRating);
			 // Delivery boy rating calculation same as restaurant
			   deliveryBoyRatingCalculation(orderRating);
		}
	}

	private void deliveryBoyRatingCalculation(OrderRating orderRating) throws NotFoundException {
		if (orderRating.getDeliveryBoyId()!=null) {
			 DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(orderRating.getDeliveryBoyId());
			 Double boyRating = deliveryBoy.getRating();
			 Long noOfBoyRating = deliveryBoy.getNoOfRating();
			 //goes one more rating to this boy so plus 1
			 if (noOfBoyRating > 0) {
				 noOfBoyRating = noOfBoyRating + 1;
			 } else {
				 noOfBoyRating = 1L;
			 }
			//now current rating and previous rating total divide by 2  for average and set to boy record
			 if (boyRating > 0) {
				 boyRating = (boyRating + orderRating.getDeliveryBoyRating())/2;
			 } else {
				 boyRating = orderRating.getDeliveryBoyRating();
			 }		 
			 deliveryBoy.setRating(boyRating);
			 deliveryBoy.setNoOfRating(noOfBoyRating);
			 deliveryBoyRepository.save(deliveryBoy);
		 }
	}


	private void vendorRatingVCalculation(OrderRating orderRating) throws NotFoundException {
		 if (orderRating.getVendorId()!= null) {
			 Vendor vendor = vendorService.getVendorDetail(orderRating.getVendorId());
			 Double vendorRating = vendor.getRating();
			 Long noOfVendorRating = vendor.getNoOfRating();
			 //goes one more rating to this vendor so plus 1
			 if (noOfVendorRating > 0) {
				 noOfVendorRating = noOfVendorRating + 1;
			 } else {
				 noOfVendorRating = 1L;
			 }
			 //now current rating and previous rating total divide by 2 for average and set to vendor record
			 if (vendorRating > 0 ) {
				 vendorRating = (vendorRating + orderRating.getVendorRating())/2;
			 } else {
				 vendorRating = orderRating.getVendorRating();
			 }			
			 vendor.setRating(vendorRating);
			 vendor.setNoOfRating(noOfVendorRating);
			 vendorRepository.save(vendor);
		 }
	}


	@Override
	public OrderRatingResponseDTO getOrderRatingbyOrderId(Long orderId) throws NotFoundException {
		 OrderRating orderRating = orderRatingRepository.findByOrderId(orderId)
		 .orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("order.rating.not.found", new Object[] {  orderId })));
		 return orderRatingMapper.toResponseDto(orderRating);
	}

}
