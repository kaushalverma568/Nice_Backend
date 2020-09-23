package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class CashCollectionDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7947238128297093026L;

	private Long id;

	@NotNull(message = "{amount.not.null}")
	private Double amount;

	private Long orderId;

	@NotNull(message = "{task.id.not.null}")
	private Long taskId;

	@NotNull(message = "{deliveryboy.id.not.null}")
	private Long deliveryboyId;

	private Boolean active;

	/**
	 * for response only
	 */
	private String deliveryBoyName;
	private String deliveryBoyNameEnglish;
	private String deliveryBoyNameArabic;

	private String customerName;

	private Date createdAt;
}
