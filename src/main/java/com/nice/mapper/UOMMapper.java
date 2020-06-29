package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.UOMDTO;
import com.nice.model.UOM;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Mar-2020
 * @description : Unit Of Measures Mapper Class which maps dtos to entity and vice versa
 */
@Component
public class UOMMapper {

	public UOMDTO toDto(final UOM uom) {
		UOMDTO uomDTO = new UOMDTO();
		BeanUtils.copyProperties(uom, uomDTO);
		return uomDTO;
	}

	public UOM toEntity(final UOMDTO uomDTO) {
		UOM uom = new UOM();
		BeanUtils.copyProperties(uomDTO, uom);
		return uom;
	}

	public List<UOMDTO> toDtos(final List<UOM> uomList) {
		List<UOMDTO> results = new ArrayList<>();
		for (UOM uom : uomList) {
			results.add(toDto(uom));
		}
		return results;
	}
}
