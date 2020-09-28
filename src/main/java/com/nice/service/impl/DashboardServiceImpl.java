/**
 *
 */
package com.nice.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.OrderStatusEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorStatus;
import com.nice.dto.DashboardCountDTO;
import com.nice.dto.OrderListFilterDto;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.SalesReportDto;
import com.nice.dto.VendorFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.repository.OrdersRepository;
import com.nice.service.CustomerService;
import com.nice.service.DashboardService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.ProductService;
import com.nice.service.TicketService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Feb-2020
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

	@Autowired
	private UserLoginService userLoginService;

	@Override
	public DashboardCountDTO getDashboardCount() throws NotFoundException, ValidationException {
//		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		DashboardCountDTO dashboardCountDTO = new DashboardCountDTO();

		/**
		 * Get count of orders
		 */
		OrderListFilterDto orderListFilterDto = new OrderListFilterDto();
		orderListFilterDto.setOrderStatus(new ArrayList<>());
		orderListFilterDto.setOrderStatusNotIn(new ArrayList<>());
		/**
		 *   total order count Consider all the status except cancelled orders.
		 */
		orderListFilterDto.setOrderStatusNotIn(Arrays.asList(OrderStatusEnum.CANCELLED.getStatusValue()));
		dashboardCountDTO.setTotalorders(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
        /**
         *  any status between order confirmation to order delivery
         */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.CONFIRMED.getStatusValue(), 
				OrderStatusEnum.IN_PROCESS.getStatusValue(), OrderStatusEnum.ORDER_IS_PREPARED.getStatusValue(),
		        OrderStatusEnum.WAITING_FOR_PICKUP.getStatusValue(), OrderStatusEnum.ORDER_PICKED_UP.getStatusValue(),
		        OrderStatusEnum.DELIVERED.getStatusValue()));
		dashboardCountDTO.setNewOrders(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 *  whose status is returned
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.RETURNED.getStatusValue()));
		dashboardCountDTO.setTotalReturend(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 *  whose status is replaced
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.REPLACED.getStatusValue()));
		dashboardCountDTO.setTotalReplaced(ordersService.getOrderCountBasedOnParams(orderListFilterDto));
		/**
		 *  whose status is Delivered
		 */
		orderListFilterDto.setOrderStatus(Arrays.asList(OrderStatusEnum.DELIVERED.getStatusValue()));
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
		VendorFilterDTO vendorFilterDTO= new VendorFilterDTO();
		vendorFilterDTO.setStatus(VendorStatus.VERIFICATION_PENDING.name());
		Long verificationPending = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
		vendorFilterDTO.setStatus(VendorStatus.NEW.name());
		Long newStatus = vendorService.getVendorCountBasedOnParams(vendorFilterDTO);
		dashboardCountDTO.setNewVendors(verificationPending + newStatus);
		
		/**
		 * Get count of delivery boys whose status is pending or verified
		 */
		dashboardCountDTO.setNewDeliveryBoys(deliveryBoyService.getCountOfNewDeliveryBoys());
		/**
		 * delivered Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysDeliveredOrder(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.DELIVERED.getStatusValue(), today));	
		/**
		 * returned Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReturedOrder(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.RETURNED.getStatusValue(), today));	
		/**
		 * replaced Order status from order history table by today's date for vendor screen
		 */
		dashboardCountDTO.setTodaysReplacedOrder(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.REPLACED.getStatusValue(), today));	
		/**
		 * pending Order status from order history table by today's date for vendor screen
		 */
		dashboardCountDTO.setTodaysplacedOrder(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.PENDING.getStatusValue(), today));	
		/**
		 * return requested Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReturedOrderRequrest(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.RETURN_REQUESTED.getStatusValue(), today));	
		/**
		 * replaced requested Order status from order history table by today's date
		 */
		dashboardCountDTO.setTodaysReplacedOrderRequest(ordersService.countByStatusAndCreatedAt(OrderStatusEnum.REPLACE_REQUESTED.getStatusValue(), today));	
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
		dashboardCountDTO.setDeliveryBoys(deliveryBoyService.getDeliveryBoyCountBasedOnParams(null));
		/**
		 * Get count of active product
		 */
		ProductParamRequestDTO productParamRequestDTO = new ProductParamRequestDTO();
		productParamRequestDTO.setActiveRecords(true);
		productParamRequestDTO.setProductVariantActiveRecords(true);
		dashboardCountDTO.setProducts(productService.getProductCountBasedOnParams(productParamRequestDTO));

		return dashboardCountDTO;
	}

	@Override
	public SalesReportDto getSalesReport(final Long vendorId, final Integer year, final String orderType) throws NotFoundException, ValidationException {
//		UserLogin userLogin = userLoginService.getUserLoginDetails(userId);
//		Long storeId = null;
//		if (Role.MANAGER.getStatusValue().equals(userLogin.getRole())) {
//			Users users = usersService.getUsersDetails(userLogin.getEntityId());
//			if (users.getStore() != null) {
//				storeId = users.getStore().getId();
//			}
//		} else if (!(Role.SUPER_ADMIN.getStatusValue().equals(userLogin.getRole()) || Role.ADMIN.getStatusValue().equals(userLogin.getRole()))) {
//			throw new ValidationException(messageByLocaleService.getMessage("invalid.role.message", null));
//		}
		return ordersRepository.getSalesReport(year, vendorId, orderType);
	}
}
