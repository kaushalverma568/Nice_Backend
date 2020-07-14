package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.DiscountDTO;
import com.nice.model.Discount;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 08-Jan-2020
 */
@Component
public class DiscountMapper {

	public DiscountDTO toDto(final Discount discount) {
		DiscountDTO discountDTO = new DiscountDTO();
		BeanUtils.copyProperties(discount, discountDTO);
		return discountDTO;
	}

	public Discount toEntity(final DiscountDTO discountDTO) {
		Discount discount = new Discount();
		BeanUtils.copyProperties(discountDTO, discount);
		return discount;
	}

	public List<DiscountDTO> toDtos(final List<Discount> discountList) {
		List<DiscountDTO> results = new ArrayList<>();
		for (Discount discount : discountList) {
			results.add(toDto(discount));
		}
		return results;
	}
}
