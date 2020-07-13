/**
 *
 */
package com.nice.service;

import java.util.List;
import java.util.Optional;

import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrdersResponseDTO;
import com.nice.exception.AuthorizationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.CartItem;
import com.nice.model.Orders;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
public interface OrdersService {

	/**
	 * @param orderRequestDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	String validateOrder(OrderRequestDTO orderRequestDto) throws ValidationException, NotFoundException;

	/**
	 * @param orderId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Orders getOrderById(Long orderId) throws NotFoundException;

	/**
	 * @param replaceCancelOrderDto
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	// void replaceOrder(ReplaceCancelOrderDto replaceCancelOrderDto, Long userId) throws NotFoundException,
	// ValidationException;

	/**
	 * @param replaceCancelOrderDto
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	// void cancelOrder(ReplaceCancelOrderDto replaceCancelOrderDto, Long userId) throws NotFoundException,
	// ValidationException;

	/**
	 * @param orderId
	 * @param isFromAdmin
	 * @return
	 * @throws NotFoundException
	 */
	OrdersResponseDTO getOrderDetails(Long orderId, boolean isFromAdmin) throws NotFoundException;

	/**
	 * @param startIndex
	 * @param pageSize
	 * @param orderListFilterDto
	 * @param forAdmin
	 * @return
	 * @throws NotFoundException
	 */
	List<OrdersResponseDTO> getOrderListBasedOnParams(Integer startIndex, Integer pageSize, OrderListFilterDto orderListFilterDto, boolean forAdmin)
			throws NotFoundException;

	/**
	 * @param deliveryBoy
	 * @return
	 */
	// List<Orders> getTodaysDeliveredOrdersForDeliveryBoy(DeliveryBoy deliveryBoy);

	/**
	 * @param deliveryBoyId
	 * @return
	 */
	// Double getTotalCashCollectionByDeliveryBoyForToday(Long deliveryBoyId);

	/**
	 * @param cartItemList
	 * @param orderRequestDto
	 * @param calculatedOrderAmt
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Orders createOrder(List<CartItem> cartItemList, OrderRequestDTO orderRequestDto, Double calculatedOrderAmt)
			throws NotFoundException, ValidationException;

	/**
	 * @param razorpayOrderId
	 * @return
	 */
	Optional<Orders> getOrderDetailsByOnlineOrderId(String razorpayOrderId);

	/**
	 * @param userId
	 * @param customerId
	 * @param userType
	 * @throws NotFoundException
	 * @throws AuthorizationException
	 */
	boolean validateUser(Long userId, Long entityId, List<String> userType) throws NotFoundException, AuthorizationException;

	/**
	 * get order count (set userId if you want to check user role also)
	 *
	 * @param orderListFilterDto
	 * @return
	 * @throws NotFoundException
	 */
	Long getOrderCountBasedOnParams(OrderListFilterDto orderListFilterDto) throws NotFoundException;

	/**
	 * get all information for app payment
	 *
	 * @param razorPayOrderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	// AppPaymentDTO getDataByRazorpayOrder(String razorPayOrderId) throws NotFoundException, ValidationException;

}
