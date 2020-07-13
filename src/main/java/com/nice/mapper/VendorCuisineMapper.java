package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.VendorCuisineDTO;
import com.nice.model.VendorCuisine;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 01-Jul-2020
 */
@Component
public class VendorCuisineMapper {

	public VendorCuisineDTO toDto(final VendorCuisine vendorCuisine) {
		VendorCuisineDTO vendorCuisineDTO = new VendorCuisineDTO();
		BeanUtils.copyProperties(vendorCuisine, vendorCuisineDTO);
		vendorCuisineDTO.setCuisineId(vendorCuisine.getCuisine().getId());
		vendorCuisineDTO.setVendorId(vendorCuisine.getVendor().getId());
		vendorCuisineDTO.setCuisineName(vendorCuisine.getCuisine().getName());
		vendorCuisineDTO.setStoreName(vendorCuisine.getVendor().getStoreName());
		vendorCuisineDTO.setVendorName(vendorCuisine.getVendor().getFirstName().concat(vendorCuisine.getVendor().getLastName()));
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
