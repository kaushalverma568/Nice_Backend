package com.nice.dto;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class PaginationUtilDto {

	private Integer startIndex;
	private Integer pageNumber;
	private Long totalPages;
	private Boolean hasPreviousPage;
	private Boolean hasNextPage;
	private Object data;

}
