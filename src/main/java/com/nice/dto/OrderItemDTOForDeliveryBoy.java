/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 04-Apr-2020
 */
@Data
@EqualsAndHashCode
public class OrderItemDTOForDeliveryBoy implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4936881575845729072L;
	private Long id;
	private String productImage;
	private String productName;
	private String uomLabel;
	private Long orderQty;
	private Long replaceQty;
}
