package com.nice.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.DashboardCountDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.SalesReportDto;
import com.nice.dto.StockDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.DashboardService;
import com.nice.service.StockDetailsService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 28-Sep-2020
 */

@RequestMapping(path = "/dashboard")
@RestController
public class DashBoardController {

	@Autowired
	private StockDetailsService stockDetailsService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private DashboardService dashboardService;

	/**
	 * Get Low stock readyMade product and fabrics
	 *
	 * @param  accessToken
	 * @param  userId
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  productType
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/lowStock/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getStockDetails(@RequestHeader("Authorization") final String accessToken,
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize) throws ValidationException, NotFoundException {
		Page<ProductVariantResponseDTO> productVariantList = stockDetailsService.getLowStockProduct( pageNumber, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("lowstock.list.message", null))
				.setData(productVariantList.getContent()).setHasNextPage(productVariantList.hasNext()).setHasPreviousPage(productVariantList.hasPrevious())
				.setTotalPages(productVariantList.getTotalPages()).setPageNumber(productVariantList.getNumber() + 1)
				.setTotalCount(productVariantList.getTotalElements()).create();
	}
	
	
	@GetMapping("/expire/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getExpireStockDetails(@RequestHeader("Authorization") final String accessToken,
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize) throws ValidationException, NotFoundException {
		Page<StockDetailsDTO> productVariantList = stockDetailsService.getExpireStockDetails( pageNumber, pageSize);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("lowstock.list.message", null))
				.setData(productVariantList.getContent()).setHasNextPage(productVariantList.hasNext()).setHasPreviousPage(productVariantList.hasPrevious())
				.setTotalPages(productVariantList.getTotalPages()).setPageNumber(productVariantList.getNumber() + 1)
				.setTotalCount(productVariantList.getTotalElements()).create();
	}

	/**
	 * Get dashBoard count
	 *
	 * @param  accessToken
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException 
	 */
	@GetMapping("/count")
	public ResponseEntity<Object> getDashboardCount(@RequestHeader("Authorization") final String accessToken)
			throws NotFoundException, ValidationException {
		DashboardCountDTO dashboardCountDTO = dashboardService.getDashboardCount();
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("dashboard.list.message", null))
				.setData(dashboardCountDTO).create();
	}

	/**
	 * Get sales report based on year
	 *
	 * @param  accessToken
	 * @param  vendorId
	 * @param  year
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/sales")
	public ResponseEntity<Object> getSalesReport(@RequestHeader("Authorization") final String accessToken,
			@RequestParam(name = "vendorId", required = false) final Long vendorId,
			@RequestParam(value = "year") final Integer year)
			throws NotFoundException, ValidationException {
		SalesReportDto salesReportDto = dashboardService.getSalesReport(vendorId, year);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK).setMessage(messageByLocaleService.getMessage("dashboard.list.message", null))
				.setData(salesReportDto).create();
	}

}
