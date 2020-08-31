package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.PincodeDTO;
import com.nice.dto.PincodeResponseDTO;
import com.nice.model.Pincode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 23-Jun-2020
 */
@Component
public class PincodeMapper {

	public PincodeResponseDTO toDto(final Pincode pincode) {
		Locale locale = LocaleContextHolder.getLocale();
		PincodeResponseDTO pincodeResponseDTO = new PincodeResponseDTO();
		BeanUtils.copyProperties(pincode, pincodeResponseDTO);
		pincodeResponseDTO.setCityId(pincode.getCity().getId());
		if (locale.getLanguage().equals("en")) {
			pincodeResponseDTO.setCityName(pincode.getCity().getNameEnglish());
		} else {
			pincodeResponseDTO.setCityName(pincode.getCity().getNameArabic());
		}
		return pincodeResponseDTO;
	}

	public Pincode toEntity(final PincodeDTO pincodeDTO) {
		Pincode pincode = new Pincode();
		BeanUtils.copyProperties(pincodeDTO, pincode);
		return pincode;
	}

	public List<PincodeResponseDTO> toDtos(final List<Pincode> pincodeList) {
		List<PincodeResponseDTO> results = new ArrayList<>();
		for (Pincode pincode : pincodeList) {
			results.add(toDto(pincode));
		}
		return results;
	}
}
