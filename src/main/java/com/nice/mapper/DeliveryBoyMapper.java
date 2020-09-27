/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.model.DeliveryBoy;
import com.nice.service.AssetService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Component
public class DeliveryBoyMapper {

	@Autowired
	private AssetService assetService;

	public DeliveryBoyResponseDTO toDto(final DeliveryBoy deliveryBoy) {
		final Locale locale = LocaleContextHolder.getLocale();
		DeliveryBoyResponseDTO deliveryBoyResponseDTO = new DeliveryBoyResponseDTO();
		BeanUtils.copyProperties(deliveryBoy, deliveryBoyResponseDTO);
		deliveryBoyResponseDTO.setRegisteredOn(deliveryBoy.getCreatedAt());
		deliveryBoyResponseDTO.setNameEnglish(deliveryBoy.getFirstNameEnglish() + " " + deliveryBoy.getLastNameEnglish());
		deliveryBoyResponseDTO.setNameArabic(deliveryBoy.getFirstNameArabic() + " " + deliveryBoy.getLastNameArabic());
		deliveryBoyResponseDTO.setBankNameEnglish(deliveryBoy.getBankNameEnglish());
		deliveryBoyResponseDTO.setAccountNameEnglish(deliveryBoy.getAccountNameEnglish());
		deliveryBoyResponseDTO.setBranchNameEnglish(deliveryBoy.getBranchNameEnglish());
		deliveryBoyResponseDTO.setBranchCityEnglish(deliveryBoy.getBranchCityEnglish());
		deliveryBoyResponseDTO.setBankNameArabic(deliveryBoy.getBankNameArabic());
		deliveryBoyResponseDTO.setAccountNameArabic(deliveryBoy.getAccountNameArabic());
		deliveryBoyResponseDTO.setBranchNameArabic(deliveryBoy.getBranchNameArabic());
		deliveryBoyResponseDTO.setBranchCityArabic(deliveryBoy.getBranchCityArabic());
		if (locale.getLanguage().equals("en")) {
			deliveryBoyResponseDTO.setFirstName(deliveryBoy.getFirstNameEnglish());
			deliveryBoyResponseDTO.setLastName(deliveryBoy.getLastNameEnglish());
			deliveryBoyResponseDTO.setName(deliveryBoy.getFirstNameEnglish() + " " + deliveryBoy.getLastNameEnglish());
			deliveryBoyResponseDTO.setBankName(deliveryBoy.getBankNameEnglish());
			deliveryBoyResponseDTO.setAccountName(deliveryBoy.getAccountNameEnglish());
			deliveryBoyResponseDTO.setBranchName(deliveryBoy.getBranchNameEnglish());
			deliveryBoyResponseDTO.setBranchCity(deliveryBoy.getBranchCityEnglish());
		} else {
			deliveryBoyResponseDTO.setFirstName(deliveryBoy.getFirstNameArabic());
			deliveryBoyResponseDTO.setLastName(deliveryBoy.getLastNameArabic());
			deliveryBoyResponseDTO.setName(deliveryBoy.getFirstNameArabic() + " " + deliveryBoy.getLastNameArabic());
			deliveryBoyResponseDTO.setBankName(deliveryBoy.getBankNameArabic());
			deliveryBoyResponseDTO.setAccountName(deliveryBoy.getAccountNameArabic());
			deliveryBoyResponseDTO.setBranchName(deliveryBoy.getBranchNameArabic());
			deliveryBoyResponseDTO.setBranchCity(deliveryBoy.getBranchCityArabic());
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoy.getProfilePictureName())) {
			deliveryBoyResponseDTO.setProfilePictureUrl(assetService.getGeneratedUrl(deliveryBoy.getProfilePictureName(), AssetConstant.DELIVERY_BOY));
		}
		if (deliveryBoy.getRating() == null) {
			deliveryBoyResponseDTO.setRating(0D);
		}
		if (deliveryBoy.getNoOfRating() == null) {
			deliveryBoyResponseDTO.setNoOfRating(0L);
		}
		return deliveryBoyResponseDTO;
	}

	public DeliveryBoy toEntity(final DeliveryBoyDTO deliveryBoyDTO) {
		DeliveryBoy deliveryBoy = new DeliveryBoy();
		BeanUtils.copyProperties(deliveryBoyDTO, deliveryBoy);
		deliveryBoy.setEmail(deliveryBoyDTO.getEmail().toLowerCase());
		return deliveryBoy;
	}

	public List<DeliveryBoyResponseDTO> toDtos(final List<DeliveryBoy> deliveryBoys) {
		List<DeliveryBoyResponseDTO> results = new ArrayList<>();
		for (DeliveryBoy deliveryBoy : deliveryBoys) {
			results.add(toDto(deliveryBoy));
		}
		return results;
	}
}
