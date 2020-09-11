package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.ToppingDTO;
import com.nice.model.Topping;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 14-07-2020
 */
@Component
public class ToppingMapper {

	public ToppingDTO toDto(final Topping topping) {
		ToppingDTO toppingResponseDTO = new ToppingDTO();
		BeanUtils.copyProperties(topping, toppingResponseDTO);
		if ("en".equals(LocaleContextHolder.getLocale().getLanguage())) {
			toppingResponseDTO.setName(toppingResponseDTO.getNameEnglish());
			toppingResponseDTO.setDescription(toppingResponseDTO.getDescriptionEnglish());
		} else {
			toppingResponseDTO.setName(toppingResponseDTO.getNameArabic());
			toppingResponseDTO.setDescription(toppingResponseDTO.getDescriptionArabic());
		}
		return toppingResponseDTO;
	}

	public Topping toEntity(final ToppingDTO toppingDTO) {
		Topping topping = new Topping();
		BeanUtils.copyProperties(toppingDTO, topping);
		return topping;
	}

	public List<ToppingDTO> toDtos(final List<Topping> toppingList) {
		List<ToppingDTO> results = new ArrayList<>();
		for (Topping Topping : toppingList) {
			results.add(toDto(Topping));
		}
		return results;
	}
}
