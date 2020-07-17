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
	
	private Date paidDate;
	
	private Double amount;

	private Long orderId;
	
	private Long  taskId;
	
	private Long  deliveryboyId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	/**
	 * for response only
	 */
	private String deliveryBoyName;
	
	private Date createdAt;
}
