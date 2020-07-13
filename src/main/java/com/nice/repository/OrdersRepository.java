/**
 *
 */
package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

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
	// @Query("Select ord from Orders ord where (ord.orderStatus=:statusValue and ord.store=:store and
	// ord.deliveryDate>=:date) or (ord.orderStatus=:secondStatus and ord.store=:store)")
	// List<Orders> findAllByOrderStatusAndVendorAndDeliveryDateGreaterThanEqualOrOrderStatus(String statusValue, Vendor
	// vendor, Date date, String secondStatus);

}
