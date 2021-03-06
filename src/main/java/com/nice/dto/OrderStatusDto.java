/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Apr-2020
 */
@Data
public class OrderStatusDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 8027111885372383244L;

	private Long id;

	private Long orderId;

	private String status;

	private Date createdAt;

	private Long createdBy;
}
