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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.OrderRatingDTO;
import com.nice.dto.OrderRatingResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.OrderRatingMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.OrderItemRating;
import com.nice.model.OrderRating;
import com.nice.model.Product;
import com.nice.model.Vendor;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.repository.OrderRatingRepository;
import com.nice.repository.ProductRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrderItemRatingService;
import com.nice.service.OrderRatingService;
import com.nice.service.ProductService;
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
	private VendorRepository vendorRepository;
	
	@Autowired
	private ProductService productService;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private DeliveryBoyService deliveryBoyService;
	
	@Autowired
	private DeliveryBoyRepository deliveryBoyRepository;
	
	@Autowired
	private MessageByLocaleService messageByLocaleService;
	
	@Autowired
	private OrderItemRatingService orderItemRatingService;

	@Override
	public OrderRatingResponseDTO addOrderRating(final OrderRatingDTO orderRatingDTO) throws NotFoundException {
		Double sum = 0.0;
		OrderRating orderRating= orderRatingMapper.toEntity(orderRatingDTO);
		/**
		 * item's rating total 
		 */
		for (int i = 0 ; i<orderRatingDTO.getOrderItemRatingList().size(); i++) {
			sum = sum + orderRatingDTO.getOrderItemRatingList().get(i).getItemRating();
		}
		// set from order vendorId & BoyId
		orderRating.setVendorId(1L);
		orderRating.setDeliveryBoyId(1L);
		
		/**
		 * for food quality item's total divide by item size (no of item)
		 */
		orderRating.setFoodQualityRating(Math.round((sum/orderRatingDTO.getOrderItemRatingList().size())*100.0)/100.0);
	
		/**
		 * for restaurant rating we total order packing rating + value of money rating + calculated food quality rating
		 * divide by 3 because its total of 3 types of rating so we get average
		 */
		orderRating.setRestaurantRating(Math.round(((orderRating.getOrderPackingRating()
				+orderRating.getValueOfMoneyRating()+orderRating.getFoodQualityRating())/3.0)*100.0)/100.0);
		/**
		 * order rating save
		 */
		orderRating = orderRatingRepository.save(orderRating);		
		/**
		 * save order item rating by adding order rating id 
		 */
		for (int i = 0 ; i<orderRatingDTO.getOrderItemRatingList().size(); i++) {
			orderRatingDTO.getOrderItemRatingList().get(i).setOrderRatingId(orderRating.getId());
			orderItemRatingService.addOrderItemRating(orderRatingDTO.getOrderItemRatingList().get(i));
		}

		return orderRatingMapper.toResponseDto(orderRating);
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
			 List<OrderItemRating> itemRatingList = orderItemRatingService.getOrderRatingByOrderRatingId(orderRatingId);
			 for (OrderItemRating orderItemRating : itemRatingList) {
				 orderItemRatingService.changeStatus(orderItemRating.getId(), active);
			}
			existingOrderRating.setActive(active);
			orderRatingRepository.save(existingOrderRating);
		}
	}

	@Override
	public Page<OrderRating> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords,String searchKeyWord) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
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
	public List<OrderRating> getOrderRatingByDeliveryBoyId (final Long deliveryBoyId){
		return  orderRatingRepository.findByDeliveryBoyId(deliveryBoyId);
	}

	@Override
	public List<OrderRating> getOrderRatingByVendorId (final Long vendorId){
		return  orderRatingRepository.findByVendorId(vendorId);
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
			 /**
			  * fetch item record for product rating average calculate
			  */
			 List<OrderItemRating> itemRatingList =  orderItemRatingService.getOrderRatingByOrderRatingId(orderRating.getId());
			 for (OrderItemRating orderItemRating : itemRatingList) {
				 //product rating calculation
				 productRatingCalculation(orderItemRating);	
			}
		}
	}

	private void productRatingCalculation(OrderItemRating orderItemRating) throws NotFoundException {
		if(orderItemRating.getProductId()!= null) {
			 Product product = productService.getProductDetail(orderItemRating.getProductId());
			 Double productRating = product.getRating();
			 Long noOfproductRating = product.getNoOfRating();
			 //goes one more rating to this boy so plus 1
			 if (noOfproductRating != null) {
				 noOfproductRating = noOfproductRating + 1;
			 } else {
				 noOfproductRating = 1L;
			 }
			 //now current rating and previous rating total divide by 2  for average and set to product record
			 if (productRating != null) {
				 productRating = (productRating + orderItemRating.getItemRating() )/2;
			 } else {
				 productRating = orderItemRating.getItemRating();
			 }				
			 product.setRating(productRating);
			 product.setNoOfRating(noOfproductRating);
			 productRepository.save(product);
		 }
		
	}


	private void deliveryBoyRatingCalculation(OrderRating orderRating) throws NotFoundException {
		if (orderRating.getDeliveryBoyId()!=null) {
			 DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(orderRating.getDeliveryBoyId());
			 Double boyRating = deliveryBoy.getRating();
			 Long noOfBoyRating = deliveryBoy.getNoOfRating();
			 //goes one more rating to this boy so plus 1
			 if (noOfBoyRating != null) {
				 noOfBoyRating = noOfBoyRating + 1;
			 } else {
				 noOfBoyRating = 1L;
			 }
			//now current rating and previous rating total divide by 2  for average and set to boy record
			 if (boyRating != null) {
				 boyRating = (boyRating + orderRating.getRestaurantRating())/2;
			 } else {
				 boyRating = orderRating.getRestaurantRating();
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
			 if (noOfVendorRating != null) {
				 noOfVendorRating = noOfVendorRating + 1;
			 } else {
				 noOfVendorRating = 1L;
			 }
			 //now current rating and previous rating total divide by 2 for average and set to vendor record
			 if (vendorRating != null) {
				 vendorRating = (vendorRating + orderRating.getRestaurantRating())/2;
			 } else {
				 vendorRating = orderRating.getRestaurantRating();
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
