/**
 *
 */
package com.nice.validator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.constant.TaskTypeEnum;
import com.nice.dto.StockAllocationDto;
import com.nice.locale.MessageByLocaleService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Component
public class StockAllocationValidator implements Validator {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return StockAllocationDto.class.equals(clazz);
	}

	@Override
	public void validate(final Object target, final Errors errors) {
		StockAllocationDto stockAllocationDto = (StockAllocationDto) target;
		if (!((TaskTypeEnum.DELIVERY.name().equalsIgnoreCase(stockAllocationDto.getAllocatedFor()))
				|| TaskTypeEnum.REPLACEMENT.name().equalsIgnoreCase(stockAllocationDto.getAllocatedFor()))) {
			errors.rejectValue("allocatedFor", "409", messageByLocaleService.getMessage("allocated.for.value", null));
		}

	}

}
