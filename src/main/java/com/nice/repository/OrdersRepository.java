/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jul-2020
 */
@Repository
public interface OrdersRepository extends JpaRepository<Orders, Long>, OrderCustomRepository {

	/**
	 * @param razorpayOrderId
	 * @return
	 */
	Optional<Orders> findByOnlineOrderId(String razorpayOrderId);

	/**
	 * get order list based on status param (This method is only used at the time of deActive store to check is order with
	 * pending status or delivered with date greater then three days from current date is present or not )
	 *
	 * @param statusValue
	 * @param tomorrowDate
	 * @param secondStatus
	 * @return
	 */
	// @Query("Select ord from Orders ord where (ord.orderStatus=:statusValue and
	// ord.store=:store and
	// ord.deliveryDate>=:date) or (ord.orderStatus=:secondStatus and
	// ord.store=:store)")
	// List<Orders>
	// findAllByOrderStatusAndVendorAndDeliveryDateGreaterThanEqualOrOrderStatus(String
	// statusValue, Vendor
	// vendor, Date date, String secondStatus);

	/**
	 * find all delivery orders for sending notification(Here only that delivery order is acceptable whose assignment try
	 * count less than 3 and timer less than current time )
	 *
	 * @param status
	 * @param assignmentTryCount
	 * @param notificationTimer
	 * @return
	 */
	List<Orders> findAllByOrderStatusAndDeliveryTypeAndAssignmentTryCountLessThanAndNotificationTimerLessThan(String status, String deliveryType,
			Integer assignmentTryCount, Date notificationTimer);

	List<Orders> findAllByOrderStatusInAndDeliveryBoyOrOrderStatusInAndReplacementDeliveryBoy(List<String> statusList, DeliveryBoy deliveryBoy,
			List<String> statusList1, DeliveryBoy replacementDeliveryBoy);

	/**
	 * @param customerId
	 * @param completedOrderStatusList
	 * @return
	 */
	@Query(value = "Select count(*) from Orders ord where ord.customer.id = :customerId and ord.orderStatus not in (:completedOrderStatusList)")
	Long getCountofOngoingOrdersForCustomer(Long customerId, List<String> completedOrderStatusList);

	/**
	 * return the orderId of customers with ongoing orders.
	 *
	 * @param customerId
	 * @param completedOrderStatusList
	 * @return
	 */
	@Query(value = "Select ord.id from Orders ord where ord.customer.id = :customerId and ord.orderStatus not in (:completedOrderStatusList)")
	Long getOrderIdOfOngoingOrdersForCustomer(Long customerId, List<String> completedOrderStatusList);
}
