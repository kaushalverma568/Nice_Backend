package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.VendorCuisineDTO;
import com.nice.model.VendorCuisine;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Component
public class VendorCuisineMapper {

	public VendorCuisineDTO toDto(final VendorCuisine vendorCuisine) {
		Locale locale = LocaleContextHolder.getLocale();
		VendorCuisineDTO vendorCuisineDTO = new VendorCuisineDTO();
		BeanUtils.copyProperties(vendorCuisine, vendorCuisineDTO);
		vendorCuisineDTO.setCuisineId(vendorCuisine.getCuisine().getId());
		vendorCuisineDTO.setVendorId(vendorCuisine.getVendor().getId());
		if (locale.getLanguage().equals("en")) {
			vendorCuisineDTO.setCuisineName(vendorCuisine.getCuisine().getNameEnglish());
			vendorCuisineDTO.setStoreName(vendorCuisine.getVendor().getStoreNameEnglish());
			vendorCuisineDTO.setVendorName(vendorCuisine.getVendor().getFirstNameEnglish().concat(" ").concat(vendorCuisine.getVendor().getLastNameEnglish()));
		} else {
			vendorCuisineDTO.setCuisineName(vendorCuisine.getCuisine().getNameArabic());
			vendorCuisineDTO.setStoreName(vendorCuisine.getVendor().getStoreNameArabic());
			vendorCuisineDTO.setVendorName(vendorCuisine.getVendor().getFirstNameArabic().concat(" ").concat(vendorCuisine.getVendor().getLastNameArabic()));
		}
		return vendorCuisineDTO;
	}

	public VendorCuisine toEntity(final VendorCuisineDTO vendorCuisineDTO) {
		VendorCuisine vendorCuisine = new VendorCuisine();
		BeanUtils.copyProperties(vendorCuisineDTO, vendorCuisine);
		return vendorCuisine;
	}

	public List<VendorCuisineDTO> toDtos(final List<VendorCuisine> vendorCuisineList) {
		List<VendorCuisineDTO> results = new ArrayList<>();
		for (VendorCuisine vendorCuisine : vendorCuisineList) {
			results.add(toDto(vendorCuisine));
		}
		return results;
	}
}
