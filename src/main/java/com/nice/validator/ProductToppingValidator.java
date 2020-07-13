/**
 *
 */
package com.nice.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.model.ProductTopping;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Component
public class ProductToppingValidator implements Validator {

	@Override
	public boolean supports(final Class<?> clazz) {
		return ProductTopping.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		// TODO Auto-generated method stub

	}

}
