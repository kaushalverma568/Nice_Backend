/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.DashboardCountDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.SalesReportDto;
import com.nice.dto.VendorFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.UserLogin;
import com.nice.repository.OrdersRepository;
import com.nice.service.CustomerService;
import com.nice.service.DashboardService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.ProductService;
import com.nice.service.TicketService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Feb-2020
 */
@Service(value = "dashboardService")
@Transactional(rollbackFor = Throwable.class)
public class DashboardServiceImpl implements DashboardService {

	private Date today = new Date();

	@Autowired
	private CustomerService customerService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Autowired
	private TicketService ticketService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private ProductService productService;

	@Autowired
	private OrdersRepository ordersRepository;


	@Override
	public DashboardCountDTO getDashboardCount() throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		DashboardCountDTO dashboardCountDTO = new DashboardCountDTO();

		/**
		 * Get count of orders
		 */
		OrderListFilterDto orderListFilterDto = new OrderListFilterDto();
		orderListFilterDto.setOrderStatus(new ArrayList<>());
		orderListFilterDto.setOrderStatusNotIn(new ArrayList<>());
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			orderListFilterDto.setVendorId(userLogin.getEntityId());
		}
		/**
		 * total order count Consider all the status except cancelled orders.
		 */
		orderListFilterDto.setOrderStatusNotIn(Arrays.asList(OrderStatusEnum.CANCELLED.getStatusValue()));
		dashboardCountDTO.setTotalorders(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * any status between order confirmation to order delivery
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.CONFIRMED.getStatusValue(),
				OrderStatusEnum.IN_PROCESS.getStatusValue(), OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue(),
				OrderStatusEnum.WAITING_FOR_PICKUP.getStatusValue(), OrderStatusEnum.ORDER_PICKED_UP.getStatusValue(),
				OrderStatusEnum.DELIVERED.getStatusValue()));
		dashboardCountDTO.setNewOrders(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is returned
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.RETURNED.getStatusValue()));
		dashboardCountDTO.setTotalReturend(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is replaced
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.REPLACED.getStatusValue()));
		dashboardCountDTO.setTotalReplaced(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is cancelled
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.CANCELLED.getStatusValue()));
		dashboardCountDTO.setTotalCancelled(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is rejected
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.REJECTED.getStatusValue()));
		dashboardCountDTO.setTotalRejected(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is Pending, Replace Requested ,Return Requested
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.PENDING.getStatusValue(),
				OrderStatusEnum.REPLACE_REQUESTED.getStatusValue(), OrderStatusEnum.RETURN_REQUESTED.getStatusValue()));
		dashboardCountDTO.setTotalPlaced(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		
		/**
		 * whose status is Confirmed, Replace Confirmed, Return Confirmed,
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.CONFIRMED.getStatusValue(),
				OrderStatusEnum.REPLACE_CONFIRMED.getStatusValue(), OrderStatusEnum.RETURN_CONFIRMED.getStatusValue()));
		dashboardCountDTO.setTotalConfirmed(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is In-Process, Order Is Prepared, Waiting for pickup, Replace Processed, Replace Order Prepared,
		 *   Replace Waiting for Picked Up, Return Processed
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.IN_PROCESS.getStatusValue(),
				OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue(), OrderStatusEnum.WAITING_FOR_PICKUP.getStatusValue(),
				OrderStatusEnum.REPLACE_PROCESSED.getStatusValue(), OrderStatusEnum.REPLACE_ORDER_PREPARED.getStatusValue(),
				OrderStatusEnum.REPLACE_WAITING_FOR_PICKUP.getStatusValue(), OrderStatusEnum.RETURN_PROCESSED.getStatusValue()));
		dashboardCountDTO.setTotalInProcess(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is Order Picked Up ,Replace Order Picked Up,Return Order Picked Up
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.ORDER_PICKED_UP.getStatusValue(),
				OrderStatusEnum.REPLACE_ORDER_PICKUP.getStatusValue(), OrderStatusEnum.RETURN_ORDER_PICKUP.getStatusValue()));
		dashboardCountDTO.setTotalPickedUp(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 * whose status is Delivered,  Replace Rejected, Return Rejected
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.DELIVERED.getStatusValue(),
				OrderStatusEnum.REPLACE_REJECTED.getStatusValue(), OrderStatusEnum.RETURN_REJECTED.getStatusValue()));
		dashboardCountDTO.setTotalDelivered(ordersService.getOrderCountBasedOnParams(orderListFilterDto));

		/**
		 * Get count of active and isAvailable delivery boys
		 */
		dashboardCountDTO.setDeliveryBoyOnField(deliveryBoyService.getCountOfOnFieldDeliveryBoy());
		/**
		 * Get count of CustomerTicket
		 */
		dashboardCountDTO.setCustomerTicket(ticketService.ticketCountBasedOnType(UserType.CUSTOMER.name()));

		/**
		 * Get count of vendors whose status is pending or verified
		 */
		VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
		vendorFilterDTO.setStatus(VendorStatus.VERIFICATION_PENDING.getStatusValue());
		Long verificationPending = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
		vendorFilterDTO.setStatus(VendorStatus.NEW.getStatusValue());
		Long newStatus = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
		dashboardCountDTO.setNewVendors(verificationPending + newStatus);

		/**
		 * Get count of delivery boys whose status is pending or verified
		 */
		dashboardCountDTO.setNewDeliveryBoys(deliveryBoyService.getCountOfNewDeliveryBoys());
			

		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
		/**
		 * delivered Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysDeliveredOrder(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.DELIVERED.getStatusValue(), today, userLogin.getEntityId()));
		/**
		 * returned Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReturedOrder(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.RETURNED.getStatusValue(), today, userLogin.getEntityId()));
		/**
		 * replaced Order status from order history table by today's date for vendor
		 * screen
		 */
		dashboardCountDTO.setTodaysReplacedOrder(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.REPLACED.getStatusValue(), today, userLogin.getEntityId()));
		/**
		 * pending Order status from order history table by today's date for vendor
		 * screen
		 */
		dashboardCountDTO.setTodaysplacedOrder(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.PENDING.getStatusValue(), today, userLogin.getEntityId()));
		/**
		 * return requested Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReturedOrderRequrest(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.RETURN_REQUESTED.getStatusValue(), today, userLogin.getEntityId()));
		/**
		 * replaced requested Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReplacedOrderRequest(
				ordersService.countByStatusAndCreatedAtAndVendorId(OrderStatusEnum.REPLACE_REQUESTED.getStatusValue(), today, userLogin.getEntityId()));
		} else {
			/**
			 * delivered Order status from order history table by today's date
			 */
			dashboardCountDTO.setTodaysDeliveredOrder(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.DELIVERED.getStatusValue(), today));
			/**
			 * returned Order status from order history table by today's date
			 */
			dashboardCountDTO.setTodaysReturedOrder(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.RETURNED.getStatusValue(), today));
			/**
			 * replaced Order status from order history table by today's date for vendor
			 * screen
			 */
			dashboardCountDTO.setTodaysReplacedOrder(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.REPLACED.getStatusValue(), today));
			/**
			 * pending Order status from order history table by today's date for vendor
			 * screen
			 */
			dashboardCountDTO.setTodaysplacedOrder(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.PENDING.getStatusValue(), today));
			/**
			 * return requested Order status from order history table by today's date
			 */
			dashboardCountDTO.setTodaysReturedOrderRequrest(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.RETURN_REQUESTED.getStatusValue(), today));
			/**
			 * replaced requested Order status from order history table by today's date
			 */
			dashboardCountDTO.setTodaysReplacedOrderRequest(
					ordersService.countByStatusAndCreatedAt(OrderStatusEnum.REPLACE_REQUESTED.getStatusValue(), today));
		}
		
		
		/**
		 * Get count of active customers
		 */
		dashboardCountDTO.setCustomers(customerService.getActiveCustomer(true));
		/**
		 * Get count of active vendors
		 */
		dashboardCountDTO.setVendors(vendorService.getActiveVendor(true));

		/**
		 * Get count of active delivery boys
		 */
		DeliveryBoyFilterDTO filterDto = new DeliveryBoyFilterDTO();
		filterDto.setActiveRecords(true);
		dashboardCountDTO.setDeliveryBoys(deliveryBoyService.getDeliveryBoyCountBasedOnParams(filterDto));

		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			/**
			 * Get count of active product
			 */
			ProductParamRequestDTO productParamRequestDTO = new ProductParamRequestDTO();
			productParamRequestDTO.setActiveRecords(true);
			productParamRequestDTO.setProductVariantActiveRecords(true);
			productParamRequestDTO.setVendorId(userLogin.getEntityId());
			dashboardCountDTO.setProducts(productService.getProductCountBasedOnParams(productParamRequestDTO));
		}
		return dashboardCountDTO;
	}

	@Override
	public SalesReportDto getSalesReport(Long vendorId, Integer year)
			throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}
		return ordersRepository.getSalesReport(year, vendorId);
	}
}
