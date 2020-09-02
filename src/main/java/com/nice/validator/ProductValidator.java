/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.ProductRequestDTO;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.ProductService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component(value = "productValidator")
public class ProductValidator implements Validator {

	@Autowired
	private ProductService productService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductRequestDTO.class.equals(clazz) || ProductParamRequestDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof ProductRequestDTO) {
			ProductRequestDTO productRequestDto = (ProductRequestDTO) target;
			/**
			 * Check if brandId or cuisineId already exists
			 */
			if (productRequestDto.getCuisineId() == null && productRequestDto.getBrandId() == null) {
				errors.rejectValue("brandId", "409", messageByLocaleService.getMessage("brand.cuisine.required", null));
			}

			/**
			 * Check for the already existing product
			 */
			try {
				if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(productRequestDto.getNameEnglish())
						&& productService.isProductExistsEnglish(productRequestDto)) {
					errors.rejectValue("nameEnglish", "409", messageByLocaleService.getMessage("english.product.already.exists", null));
				}
				if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(productRequestDto.getNameArabic())
						&& productService.isProductExistsArabic(productRequestDto)) {
					errors.rejectValue("nameArabic", "409", messageByLocaleService.getMessage("arabic.product.already.exists", null));
				}
			} catch (ValidationException e) {
				errors.rejectValue("nameEnglish", "409", e.getMessage());
			}
		}
	}

}
