/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Aug-2020
 */
@Data
public class ProductAttributeResponseDTO implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -6935368355186006892L;
	private String attributeName;
	private List<ProductAttributeValueDTO> productAttributeValueList;
}
