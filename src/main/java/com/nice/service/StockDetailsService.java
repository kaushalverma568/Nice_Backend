package com.nice.service;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;

import com.nice.dto.AddStockDto;
import com.nice.dto.AddStockRequestDTO;
import com.nice.dto.LotwiseStockRequestDTO;
import com.nice.dto.StockDetailFilterDTO;
import com.nice.dto.StockDetailsDTO;
import com.nice.dto.StockTransferDto;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductVariant;
import com.nice.model.StockDetails;
import com.nice.model.Vendor;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Dec-2019
 */
public interface StockDetailsService {

	/**
	 * Get details of Stock Dto by id 
	 *
	 * @param  countryId
	 * @return
	 * @throws NotFoundException
	 */
	StockDetailsDTO getStockDetails(Long stockDetailsId) throws NotFoundException;

	/**
	 * Get Stock details based on Id : Specially for internally calls
	 *
	 * @param  countryId
	 * @return
	 * @throws NotFoundException
	 */
	StockDetails getStockDetailsDetails(Long stockDetailsId) throws NotFoundException;

	/**
	 * used for stock transfer
	 * 
	 * @param  stockTransferDto
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	StockDetails updateStockDetails(StockTransferDto stockTransferDto) throws NotFoundException, ValidationException;

	/**
	 * entity to DTO
	 * 
	 * @param  stockDetails
	 * @return
	 * @throws NotFoundException
	 */
	StockDetailsDTO fetchStockInfo(StockDetails stockDetails) throws NotFoundException;

	/**
	 * get available from product id and uom id 
	 * 
	 * @param  productId
	 * @param  uomId
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Double getStockDetailsDtoForProductAndUom(Long productId, Long uomId, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * export filtered list
	 * 
	 * @param  httpServletResponse
	 * @param  stockDetailFilterDTO
	 * @throws NotFoundException
	 * @throws FileNotFoundException 
	 * @throws IOException
	 */
	void exportStockDetailsList(HttpServletResponse httpServletResponse, StockDetailFilterDTO stockDetailFilterDTO) throws NotFoundException, FileNotFoundException;


	/**
	 * get List by pagination
	 * 
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  stockDetailFilterDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<StockDetailsDTO> getStockDetailsList(Integer pageNumber, Integer pageSize, StockDetailFilterDTO stockDetailFilterDTO)
			throws NotFoundException, ValidationException;

	
	/**
	 * get stock detail from sku
	 * 
	 * @param  sku
	 * @return
	 * @throws NotFoundException
	 */
	List<Long> getStockDetailsForSku(String sku) throws NotFoundException;

	/**
	 * get lot no list by vendor and product variant
	 *
	 * @param  vendorId
	 * @param  productVariantId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<Long> getLotNoListByVendorAndProductVariant(Long vendorId, Long productVariantId) throws NotFoundException, ValidationException;

	/**
	 * get lot no list by product variant
	 * 
	 * @param  vendorId
	 * @param  productVariantId
	 * @return
	 */
	List<Long> getLotNosWithAvailableQtyFromProductVariant(Long vendorId, Long productVariantId);

	/**
	 * for expire schedular run
	 * 
	 * @param runDate
	 */
	void moveQtyToExpiredState(Date runDate);

	/**
	 * stock details from product variant and vendor 
	 * 
	 * @param productvariant
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<StockDetails> getStockDetailsForProductVariant(ProductVariant productvariant, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * stock detail from lot no 
	 * 
	 * @param productvariant
	 * @param vendorId
	 * @param lotNo
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	StockDetails getStockDetailsForProductVarientAndLot(ProductVariant productvariant, Long vendorId, Long lotNo) throws NotFoundException, ValidationException;

	/**
	 * add Stock
	 * 
	 * @param addStockDto
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	StockDetailsDTO addStockDetails(AddStockDto addStockDto) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param productVariant
	 * @param vendorId
	 * @param lotNo
	 * @return
	 * @throws NotFoundException
	 */
	StockDetails getStockDetailsForProductVarientAndLotAndVendor(ProductVariant productVariant, Long vendorId,Long lotNo) throws NotFoundException;

	/**
	 * Available count from product variant  
	 * 
	 * @param productVariant
	 * @return
	 */
	Long getCountForVariantForVendor(ProductVariant productVariant);

	/**
	 *  AddStockRequestDTO details  to add stock
	 * 
	 * @param addStockRequestDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addStockDetails(AddStockRequestDTO addStockRequestDTO)	throws NotFoundException, ValidationException;

	/**
	 * delete stock
	 * 
	 * @param productId
	 * @param uomId
	 * @param vendorId
	 * @param lotNo
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteStock(Long productId, Long uomId, Long vendorId, Long lotNo) throws NotFoundException, ValidationException;

	/**
	 * get variant from product Id and Uom Id 
	 * 
	 * @param productId
	 * @param uomId
	 * @return
	 * @throws NotFoundException
	 */
	ProductVariant getproductVariantFromProductIdAndUomId(Long productId, Long uomId) throws NotFoundException;

	/**
	 * validation method required from validator
	 * 
	 * @param stockRequestDTOs
	 * @param vendor
	 * @param productVariant
	 * @return
	 * @throws ValidationException
	 */
	boolean validateAddStockDetails(List<LotwiseStockRequestDTO> stockRequestDTOs, Vendor vendor, ProductVariant productVariant) throws ValidationException;

}
