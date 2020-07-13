/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 07-Jul-2020
 */
@Data
public class OrderListFilterDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 6362437730588459181L;
	private Long vendorId;
	private Long deliveryBoyId;
	private List<String> orderStatus;
	private Date orderDate;
	private String searchKeyword;
	private Long customerId;
	private Date replacedOn;
	private String paymentMode;
	private Date deliveryDate;
	private Boolean isForPaymentTransaction = false;
	private Date paymentDate;
	private Long replacementDeliveryBoyId;
}
