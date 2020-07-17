/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Jul-2020
 */
@Data
public class PaymentDetailDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 8368463180306692122L;

	private Long id;
	@NotNull(message = "delivery.boy.not.null")
	private Long deliveryBoyId;
	@NotNull(message = "transaction.not.null")
	private Long transactionId;
	@NotNull(message = "payment.amount.not.null")
	private Double paymentAmount;
	@NotNull(message = "active.not.null")
	private Boolean active;

	@Temporal(TemporalType.DATE)
	private Date paymentDate;
}
