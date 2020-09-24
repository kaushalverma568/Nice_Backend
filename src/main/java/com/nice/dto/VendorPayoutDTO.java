/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */

@Data
public class VendorPayoutDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5990549155402221395L;

	private Long id;

	private String name;

	private String storeName;

	private String businessCategoryName;

	private String phoneNumber;

	private String storePhoneNumber;

	private Date registeredOn;

	private Long businessCategoryId;

	private Integer cartOrders;

	private Integer replacementOrders;

	private Integer returnOrders;

	private Integer totalAttended;

	private Date lastPaymentOn;

	private Double totalPaid;

	public VendorPayoutDTO(final Long id, final String name, final String storeName, final String businessCategoryName, final String phoneNumber,
			final String storePhoneNumber, final Date registeredOn, final Long businessCategoryId, final Integer cartOrders, final Integer replacementOrders,
			final Integer returnOrders, final Integer totalAttended, final Date lastPaymentOn, final Double totalPaid) {
		super();
		this.id = id;
		this.name = name;
		this.storeName = storeName;
		this.businessCategoryName = businessCategoryName;
		this.phoneNumber = phoneNumber;
		this.storePhoneNumber = storePhoneNumber;
		this.registeredOn = registeredOn;
		this.businessCategoryId = businessCategoryId;
		this.cartOrders = cartOrders;
		this.replacementOrders = replacementOrders;
		this.returnOrders = returnOrders;
		this.totalAttended = totalAttended;
		this.lastPaymentOn = lastPaymentOn;
		this.totalPaid = totalPaid;
	}

}