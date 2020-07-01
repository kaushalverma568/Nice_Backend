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
import com.nice.service.ProductService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component(value = "productValidator")
public class ProductValidator implements Validator {

	@Autowired
	private ProductService productService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductRequestDTO.class.equals(clazz) || ProductParamRequestDTO.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		if (target instanceof ProductRequestDTO) {
			ProductRequestDTO productRequestDto = (ProductRequestDTO) target;
			productService.isProductExists(productRequestDto);
		}
	}

}
