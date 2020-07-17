package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 15-07-2020
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class PaymentDetailsDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -7076163196625869062L;

	private Long id;

	@NotNull(message = "{task.ids.not.null}")
	private List<Long> taskIds;

	@NotNull(message = "{paidOn.not.null}")
	private Date paidOn;

	@NotNull(message = "{payment.amount.not.null}")
	private Double paymentAmount;

	@NotBlank(message = "{transactionNo.not.null}")
	private String transactionNo;
}