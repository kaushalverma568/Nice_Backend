package com.nice.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Data
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

	@NotEmpty(message = "{transactionNo.not.null}")
	private String transactionNo;
}