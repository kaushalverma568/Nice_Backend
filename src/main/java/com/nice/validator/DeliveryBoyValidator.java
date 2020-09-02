package com.nice.validator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.locale.MessageByLocaleService;
import com.nice.service.DeliveryBoyService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 19, 2020
 */
@Component
public class DeliveryBoyValidator implements Validator {
	/**
	 *
	 */
	private static final String EMAIL = "email";

	/**
	 *
	 */
	private static final String DELIVERY_BOY_EMAIL_NOT_UNIQUE = "deliveryBoy.email.not.unique";

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DeliveryBoyValidator.class);

	/**
	 * Locale message service - to display response messages from messages_en_US.properties
	 */
	@Autowired
	private MessageByLocaleService messageByLocaleService;

	/**
	 * service - to implement business logic
	 */
	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Override
	public boolean supports(final Class<?> clazz) {
		return DeliveryBoyDTO.class.equals(clazz) || DeliveryBoyPersonalDetailsDTO.class.equals(clazz) || DeliveryBoyAccountDetailsDTO.class.equals(clazz)
				|| DeliveryBoyFilterDTO.class.equals(clazz);
	}

	/**
	 * purpose - to validate object and apply various validations. this method may carry number of validation conditions.
	 */

	@Override
	public void validate(final Object target, final Errors errors) {
		DeliveryBoyDTO deliveryBoyDTO = null;
		if (target instanceof DeliveryBoyDTO) {
			deliveryBoyDTO = (DeliveryBoyDTO) target;
		} else if (target instanceof DeliveryBoyPersonalDetailsDTO) {
			DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO = (DeliveryBoyPersonalDetailsDTO) target;
			deliveryBoyDTO = new DeliveryBoyDTO();
			BeanUtils.copyProperties(deliveryBoyPersonalDetailsDTO, deliveryBoyDTO);
		} else {
			LOGGER.info("target is not instance of DeliveryBoyDTO and ");
		}
		// to check deliveryBoy email duplication
		if (deliveryBoyDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoyDTO.getEmail())
				&& deliveryBoyService.isDeliveryBoyExists(deliveryBoyDTO).booleanValue()) {
			errors.rejectValue(EMAIL, "409", messageByLocaleService.getMessage(DELIVERY_BOY_EMAIL_NOT_UNIQUE, null));
		}
		// to check deliveryBoy phone duplication
		if (deliveryBoyDTO != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoyDTO.getPhoneNumber())
				&& deliveryBoyService.isPhoneNumberExists(deliveryBoyDTO).booleanValue()) {
			errors.rejectValue("phoneNumber", "409", messageByLocaleService.getMessage("deliveryboy.phone.exists", null));
		}

	}

}
