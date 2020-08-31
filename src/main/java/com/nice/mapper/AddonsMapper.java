package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.AddonsDTO;
import com.nice.model.Addons;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Jul-2020
 */
@Component
public class AddonsMapper {

	public AddonsDTO toDto(final Addons addons) {
		AddonsDTO addonsDTO = new AddonsDTO();
		BeanUtils.copyProperties(addons, addonsDTO);
		addonsDTO.setVendorId(addons.getVendor().getId());
		return addonsDTO;
	}

	public Addons toEntity(final AddonsDTO addonsDTO) {
		Addons addons = new Addons();
		BeanUtils.copyProperties(addonsDTO, addons);
		return addons;
	}

	public List<AddonsDTO> toDtos(final List<Addons> addons) {
		List<AddonsDTO> results = new ArrayList<>();
		for (Addons addons2 : addons) {
			results.add(toDto(addons2));
		}
		return results;
	}
}
