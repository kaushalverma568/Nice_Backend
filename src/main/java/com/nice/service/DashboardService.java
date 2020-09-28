/**
 *
 */
package com.nice.service;

import com.nice.dto.DashboardCountDTO;
import com.nice.dto.SalesReportDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Feb-2020
 */
public interface DashboardService {

	/**
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException 
	 */
	DashboardCountDTO getDashboardCount() throws NotFoundException, ValidationException;

	/**
	 * Get sales report
	 *
	 * @param  vendorId
	 * @param  year
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	SalesReportDto getSalesReport(Long vendorId, Integer year, String orderType) throws NotFoundException, ValidationException;


}
