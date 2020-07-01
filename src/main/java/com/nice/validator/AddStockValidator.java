package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.AddStockRequestDTO;
import com.nice.dto.StockDetailFilterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.ProductVariant;
import com.nice.model.Vendor;
import com.nice.repository.StockDetailsRepository;
import com.nice.service.ProductVariantService;
import com.nice.service.StockDetailsService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 31-Dec-2019
 */
@Component
public class AddStockValidator implements Validator {

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(AddStockValidator.class);

	@Autowired
	private VendorService vendorService;

	@Autowired
	StockDetailsService stockDetailsService;

	@Autowired
	StockDetailsRepository stockDetailsRepository;

	@Autowired
	private ProductVariantService productVariantService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return AddStockRequestDTO.class.equals(clazz) || StockDetailFilterDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof AddStockRequestDTO) {
			final AddStockRequestDTO addStockRequestDTO = (AddStockRequestDTO) target;
			/**
			 * Validate the AddStockObject
			 */
			Vendor vendor = null;
			try {
				if (addStockRequestDTO.getVendorId() != null) {
					vendor = vendorService.getVendorDetail(addStockRequestDTO.getVendorId());
				}
			} catch (NotFoundException e1) {
				errors.rejectValue("vendorId", "409",
						messageByLocaleService.getMessage("vendor.not.found", new Object[] { addStockRequestDTO.getVendorId() }));
			}
			if (vendor != null) {
				skuValidation(errors, addStockRequestDTO, vendor);
			}
			
		}
	}

	/**
	 * @param errors
	 * @param addStockRequestDTO
	 * @param vendor
	 */
	private void skuValidation(final Errors errors, final AddStockRequestDTO addStockRequestDTO, Vendor vendor) {
		ProductVariant productVariant = null;
		try {
			if (addStockRequestDTO.getSku() != null) {
				productVariant = productVariantService.getProductVariantDetailBySku(addStockRequestDTO.getSku(),addStockRequestDTO.getVendorId());
				if (!productVariant.getActive().booleanValue()) {
					errors.rejectValue("sku", "409",
							messageByLocaleService.getMessage("cannot.add.stock.deactive.product", new Object[] { addStockRequestDTO.getSku() }));
				}
			}
		} catch (NotFoundException e1) {
			errors.rejectValue("sku", "409",
					messageByLocaleService.getMessage("unknown.sku", new Object[] { addStockRequestDTO.getSku() }));
		}
		try {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(addStockRequestDTO.getStockRequestDTOs())) {
				if (stockDetailsService.validateAddStockDetails(addStockRequestDTO.getStockRequestDTOs(), vendor, productVariant)) {
					errors.rejectValue("sku", "409", messageByLocaleService.getMessage("lotNo.not.unique", null));
				}
			} else {
				errors.rejectValue("sku", "409", messageByLocaleService.getMessage("stock.not.null", null));
			}
		} catch (ValidationException e) {
			LOGGER.error(e.getMessage());
		}
	}
}
