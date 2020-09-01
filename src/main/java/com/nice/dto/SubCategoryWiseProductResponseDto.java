/**
 *
 */
package com.nice.dto;

import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Aug-2020
 */
@Data
public class SubCategoryWiseProductResponseDto {

	private String subCategoryName;
	private List<ProductResponseDTO> productResponseList;
}
