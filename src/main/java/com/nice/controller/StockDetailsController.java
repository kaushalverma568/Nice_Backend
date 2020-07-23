package com.nice.controller;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.nice.dto.AddStockRequestDTO;
import com.nice.dto.StockDetailFilterDTO;
import com.nice.dto.StockDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.response.GenericResponseHandlers;
import com.nice.service.StockDetailsService;
import com.nice.validator.AddStockValidator;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 30-Jan-2020
 */
@RequestMapping(path = "/stock")
@RestController
public class StockDetailsController {

	private static final Logger LOGGER = LoggerFactory.getLogger(StockDetailsController.class);
	/**
	 * Locale message service - to display response messages from Property file
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * validator - to apply/check any type of validation regarding state
	 */
	@Autowired
	private AddStockValidator stockDetailsValidator;

	/**
	 * Bind validator with object using 'BindingResult' in method
	 *
	 * @param binder
	 */
	@InitBinder
	public void initialiseBinder(final WebDataBinder binder) {
		binder.addValidators(stockDetailsValidator);
	}

	@Autowired
	private StockDetailsService stockDetailsService;

	/**
	 * Add Stock
	 * 
	 * @param accessToken
	 * @param addStockRequestDTO
	 * @param bindingResult
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	@PostMapping
	public ResponseEntity<Object> addStock(@RequestHeader("Authorization") final String accessToken,
			@RequestBody @Valid final AddStockRequestDTO addStockRequestDTO, final BindingResult bindingResult) throws ValidationException, NotFoundException {
		LOGGER.info("Inside add stock {}", addStockRequestDTO);
		List<FieldError> fieldErrors = bindingResult.getFieldErrors();
		if (!fieldErrors.isEmpty()) {
			throw new ValidationException(fieldErrors.stream().map(FieldError::getDefaultMessage).collect(Collectors.joining(",")));
		}
		stockDetailsService.addStockDetails(addStockRequestDTO);
		LOGGER.info("Outside add stock");
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.create.message", null)).create();
	}

	/**
	 * Get List 
	 * 
	 * @param accessToken
	 * @param pageNumber
	 * @param pageSize
	 * @param stockDetailFilterDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@PostMapping("/pageNumber/{pageNumber}/pageSize/{pageSize}")
	public ResponseEntity<Object> getStockDetails(@RequestHeader("Authorization") final String accessToken, 
			@PathVariable final Integer pageNumber, @PathVariable final Integer pageSize, @RequestBody final StockDetailFilterDTO stockDetailFilterDTO)
			throws NotFoundException, ValidationException {
		Page<StockDetailsDTO> stockDetailsList = stockDetailsService.getStockDetailsList(pageNumber, pageSize, stockDetailFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.list.message", null)).setData(stockDetailsList.getContent())
				.setHasNextPage(stockDetailsList.hasNext()).setHasPreviousPage(stockDetailsList.hasPrevious()).setTotalPages(stockDetailsList.getTotalPages())
				.setPageNumber(stockDetailsList.getNumber() + 1).setTotalCount(stockDetailsList.getTotalElements()).create();
	}

	/**
	 * Get available qty for a product variant
	 *
	 * @param productId
	 * @param uomId
	 * @param productType
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping
	public ResponseEntity<Object> getStockAvailableUsingProductIdAndUomId(@RequestParam(value = "productId", required = true) final Long productId,
			@RequestParam(value = "uomId", required = true) final Long uomId, @RequestParam(value = "vendorId", required = true) final Long vendorId)
			throws NotFoundException, ValidationException {
		Double availableQty = stockDetailsService.getStockDetailsDtoForProductAndUom(productId, uomId, vendorId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("qty.message", null)).setData(availableQty).setStatus(HttpStatus.OK)
				.create();
	}
	
	/**
	 * delete Stock
	 * 
	 * @param productId
	 * @param lotNo
	 * @param uomId
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@DeleteMapping
	public ResponseEntity<Object> deleteStock(@RequestParam(value = "productId", required = true) final Long productId, @RequestParam(value = "lotNo", required = true) final Long lotNo,
			@RequestParam(value = "uomId", required = true) final Long uomId, @RequestParam(value = "vendorId", required = true) final Long vendorId)
			throws NotFoundException, ValidationException {
		stockDetailsService.deleteStock(productId, uomId, vendorId, lotNo);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("delete.message", null)).setStatus(HttpStatus.OK)
				.create();
	}

	/**
	 *  Export List
	 *  
	 * @param accessToken
	 * @param userId
	 * @param stockDetailFilterDTO
	 * @param uomId
	 * @param httpServletResponse
	 * @return
	 * @throws NotFoundException
	 * @throws IOException
	 */
	@PostMapping(value = "/export/list", produces = "text/csv")
	public ResponseEntity<Object> exportStockSummary(@RequestHeader("Authorization") final String accessToken,
			@RequestBody final StockDetailFilterDTO stockDetailFilterDTO, final Long uomId, final HttpServletResponse httpServletResponse)
			throws NotFoundException, IOException {
		stockDetailsService.exportStockDetailsList(httpServletResponse, stockDetailFilterDTO);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.list.message", null)).create();
	}

	/**
	 * get Stock detail by Sku
	 * 
	 * @param accessToken
	 * @param sku
	 * @return
	 * @throws NotFoundException
	 */
	@GetMapping("/sku/{sku}")
	public ResponseEntity<Object> getStockDetailsForSku(@RequestHeader("Authorization") final String accessToken, @PathVariable(name = "sku") final String sku)
			throws NotFoundException {
		List<Long> lotNoList = stockDetailsService.getStockDetailsForSku(sku);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.list.message", null)).setData(lotNoList)
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * Get lot no list by vendor and product variant (Only use for pos screen)
	 *
	 * @param accessToken
	 * @param vendorId
	 * @param productVariantId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@GetMapping("/vendorId/{vendorId}/variant/{productVariantId}")
	public ResponseEntity<Object> getStockDetailsByVendorAndVariant(@RequestHeader("Authorization") final String accessToken,
			@PathVariable(name = "vendorId") final Long vendorId, @PathVariable(name = "productVariantId") final Long productVariantId)
			throws NotFoundException, ValidationException {
		List<Long> lotNoList = stockDetailsService.getLotNoListByVendorAndProductVariant(vendorId, productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.list.message",null)).setData(lotNoList)
				.setStatus(HttpStatus.OK).create();
	}

	/**
	 * Get lot no list by vendor and product variant with available > 0
	 * 
	 * @param accessToken
	 * @param vendorId
	 * @param productVariantId
	 * @return
	 */
	@GetMapping("/lot/vendorId/{vendorId}/variant/{productVariantId}")
	public ResponseEntity<Object> getAvailableLotNosByVendorAndVariant(@RequestHeader("Authorization") final String accessToken,
			@PathVariable(name = "vendorId") final Long vendorId, @PathVariable(name = "productVariantId") final Long productVariantId) {
		List<Long> lotNoList = stockDetailsService.getLotNosWithAvailableQtyFromProductVariant(vendorId, productVariantId);
		return new GenericResponseHandlers.Builder().setStatus(HttpStatus.OK)
				.setMessage(messageByLocaleService.getMessage("stock.detail.list.message",null)).setData(lotNoList)
				.setStatus(HttpStatus.OK).create();
	}
	
	
}
