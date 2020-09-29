/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Feb-2020
 */
@Data
public class DashboardCountDTO implements Serializable {
	/**
	* 
	*/
	private static final long serialVersionUID = -4348916934051775474L;

	private Long totalorders;   // total order count Consider all the status except cancelled orders.
	private Long customers;     // only Active Customers
	private Long vendors;      // only Active Vendors
	private Long deliveryBoys; // only Active Delivery Boys
	private Long products;      // active product count
	
	
	private Long newOrders;        // any status between order confirmation to order delivery
	private Long newVendors;       // whose status is pending or verified
	private Long newDeliveryBoys; // whose status is pending or verified
	

	private Long totalReturend;    //whose status is returned
	private Long totalReplaced;    //whose status is replaced
	private Long totalDelivered;   //whose status is Delivered

	private Long deliveryBoyOnField; // isAvailable and active true
	private Long customerTicket;     // filter by customer type
	
	private Long todaysReplacedOrderRequest;   // Replace Order Request status from order history table by today's date
	private Long todaysReturedOrderRequrest;  // Replace Order Request status from order history table by today's date
	private Long todaysDeliveredOrder; // Replace Order Request status from order history table by today's date
	private Long todaysplacedOrder;    // Replace Order Request status from order history table by today's date
	
	private Long todaysReplacedOrder;   // Replaced Order status from order history table by today's date
	private Long todaysReturedOrder;  // Replaced Order status from order history table by today's date
		
	
	/**
	 * for bar chart
	 */
	private Long totalPlaced; //
	private Long totalRejected; //
	private Long totalCancelled;  // cancelled
	private Long totalConfirmed; //
	private Long totalInProcess; // 
	private Long totalPickedUp;
	

}
