/**
 *
 */
package com.nice.service;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import com.nice.dto.OrderListFilterDto;
import com.nice.dto.OrderRequestDTO;
import com.nice.dto.OrdersResponseDTO;
import com.nice.dto.ReplaceCancelOrderDto;
import com.nice.exception.AuthorizationException;
import com.nice.exception.FileNotFoundException;
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
	// void replaceOrder(ReplaceCancelOrderDto replaceCancelOrderDto, Long userId)
	// throws NotFoundException,
	// ValidationException;

	/**
	 * @param replaceCancelOrderDto
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	// void cancelOrder(ReplaceCancelOrderDto replaceCancelOrderDto, Long userId)
	// throws NotFoundException,
	// ValidationException;

	/**
	 * @param orderId
	 * @param isFromAdmin
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	OrdersResponseDTO getOrderDetails(Long orderId) throws NotFoundException, ValidationException;

	/**
	 * @param startIndex
	 * @param pageSize
	 * @param orderListFilterDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<OrdersResponseDTO> getOrderListBasedOnParams(Integer startIndex, Integer pageSize, OrderListFilterDto orderListFilterDto)
			throws NotFoundException, ValidationException;

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
	Orders createOrder(List<CartItem> cartItemList, OrderRequestDTO orderRequestDto, Double calculatedOrderAmt) throws NotFoundException, ValidationException;

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
	 * @throws ValidationException
	 */
	Long getOrderCountBasedOnParams(OrderListFilterDto orderListFilterDto) throws NotFoundException, ValidationException;

	/**
	 * @param replaceCancelOrderDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void cancelOrder(ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException;

	/**
	 * @param newStatus
	 * @param order
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(String newStatus, Orders order) throws NotFoundException, ValidationException;

	/**
	 *
	 * @param httpServletResponse
	 * @param orderListFilterDto
	 * @throws NotFoundException
	 * @throws FileNotFoundException
	 */
	void exportOrderList(HttpServletResponse httpServletResponse, OrderListFilterDto orderListFilterDto) throws NotFoundException, FileNotFoundException;

	/**
	 * get all qualified orders for sending accept order notification
	 *
	 * @param status
	 * @param assignmentTryCount
	 * @param notificationTimer
	 * @return
	 */
	List<Orders> getAllQualifiedDeliveryOrdersForSendingNotification(String status, String deliveryType, Integer assignmentTryCount, Date notificationTimer);

	/**
	 * replace order
	 *
	 * @param replaceCancelOrderDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void replaceOrder(ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException;

	/**
	 * return order
	 *
	 * @param replaceCancelOrderDto
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void returnOrder(ReplaceCancelOrderDto replaceCancelOrderDto) throws ValidationException, NotFoundException;

	/**
	 * this method is used for change status to ready
	 *
	 * @param ordersId
	 * @param status
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long ordersId, String status) throws NotFoundException, ValidationException;

	/**
	 * @param replaceCancelOrderDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void rejectOrder(ReplaceCancelOrderDto replaceCancelOrderDto) throws NotFoundException, ValidationException;

	/**
	 * @param orderId
	 * @throws NotFoundException
	 */
	List<String> getNextStatus(Long orderId) throws NotFoundException;

	/**
	 * get all information for app payment
	 *
	 * @param razorPayOrderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	// AppPaymentDTO getDataByRazorpayOrder(String razorPayOrderId) throws
	// NotFoundException, ValidationException;

}
