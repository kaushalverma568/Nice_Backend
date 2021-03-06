/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Data
public class OrdersResponseDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = -8799119443604713278L;

	private Long id;
	private Long customerId;
	private String customerName;
	private String phoneNumber;

	/**
	 * This field contains the amount paid by the customer (payable amount)
	 */
	private Double totalOrderAmount;
	/**
	 * This field contains the amount of order after discount, not including delivery charge
	 */
	private Double grossOrderAmount;
	private String paymentMode;
	private String orderStatus;
	private Date createdAt;
	private Date deliveryDate;
	private String vendorName;
	private String address;
	private Double deliveryCharge;
	private String deliveryBoyName;
	private String deliveryBoyNameEnglish;
	private String deliveryBoyNameArabic;
	private Long deliveryBoyId;
	private String cancelReason;
	private String rejectReason;
	private String returnReason;
	private String replaceReason;
	private List<OrderItemResponseDTO> orderItemResponseDtoList;
	private Long count;
	private String description;
	private String email;
	private Long vendorId;
	private List<OrderStatusDto> orderStatusDtoList;
	private String replacementDeliveryBoyName;
	private String replacementDeliveryBoyNameEnglish;
	private String replacementDeliveryBoyNameArabic;
	private Long replacementDeliveryBoyId;
	private String vendorImageUrl;
	private Integer assignmentTryCount;

	private String businessCategoryId;
	private String businessCategoryName;
	private String businessCategoryNameArabic;
	private String businessCategoryNameEnglish;

	private String deliveryBoyPhoneNumber;

	private String replacementDeliveryBoyPhoneNumber;

	private String cancelReturnReplaceDescription;

	private String returnReplaceRequestCancelRejectDescription;
	private String returnReplaceRequestReason;
	/**
	 * city field is added for set city in email templates
	 */
	private String city;
	/**
	 * area field is added for set area in email and push notification templates
	 */
	private String area;

	/**
	 * This field will contain the count of items in the order for list display, this will include all the main product
	 * variants, excluding extras, addons, product attributes, toppings
	 */
	private Long itemCount;

	private Boolean manageInventory;
	private Date cancelDate;
	private String cancelledBy;
	private String vendorPhoneNumber;
	private String deliveryType;

	/**
	 * order rating
	 */
	private List<RatingQuestionDTO> ratingQuestionList;
	private OrderRatingResponseDTO orderRating;

	/**
	 * online payment
	 */
	String hesabeOrderId;
	String paymentId;
	String paymentToken;
	Double administrativeCharge;

	/**
	 *
	 */
	private Date paymentDate;

	/**
	 * true if the vendor is configure for replace order, if false that means the vendor has configuration related to
	 * return.
	 */
	private Boolean canReplace;
	private Boolean canReturn;

	/**
	 * This flag determines if the order is refunded
	 */
	private Boolean refunded;

	private Double walletContribution;

	private String customerShippingName;

	private String customerShippingPhoneNumber;

	/**
	 * latitude of vendor and customer
	 */
	private BigDecimal vendorLatitude;

	private BigDecimal vendorLongitude;

	private BigDecimal latitude;

	private BigDecimal longitude;

	private String orderType;

	/**
	 * This contains the order amount for the customer : grossOrderAmount + delivery charge
	 */
	private Double customerTotalOrderAmount;
}
