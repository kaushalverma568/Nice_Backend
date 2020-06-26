package com.nice.util;

import com.nice.dto.PaginationUtilDto;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public class PaginationUtil {

	private PaginationUtil() {
	}

	/**
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  totalCount
	 * @return
	 * @throws ValidationException
	 */
	public static PaginationUtilDto calculatePagination(final Integer pageNumber, final Integer pageSize, final long totalCount) throws ValidationException {
		if ((pageNumber == null) || (pageSize == null) || (pageNumber == 0) || (pageSize == 0)) {
			throw new ValidationException("pageNumber or pageSize can not be null or Zero");
		}
		PaginationUtilDto paginationUtilDto = new PaginationUtilDto();
		boolean hasPreviousPage = false;
		boolean hasNextPage = false;
		long totalPages = totalCount / pageSize;
		if ((totalCount % pageSize) > 0) {
			totalPages += 1;
		}
		if ((pageNumber != 1) && (pageNumber <= totalPages)) {
			hasPreviousPage = true;
		}
		if (pageNumber < totalPages) {
			hasNextPage = true;
		}

		paginationUtilDto.setStartIndex((pageNumber - 1) * pageSize);
		paginationUtilDto.setTotalPages(totalPages);
		paginationUtilDto.setPageNumber(pageNumber);
		paginationUtilDto.setHasPreviousPage(hasPreviousPage);
		paginationUtilDto.setHasNextPage(hasNextPage);
		return paginationUtilDto;

	}

}
