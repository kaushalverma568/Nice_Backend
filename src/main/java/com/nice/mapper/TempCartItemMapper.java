package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CartItemResponseDTO;
import com.nice.dto.TempCartItemDTO;
import com.nice.model.TempCartItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@Component
public class TempCartItemMapper {

	public CartItemResponseDTO toDto(final TempCartItem tempCartItem) {
		CartItemResponseDTO cartItemResponseDTO = new CartItemResponseDTO();
		BeanUtils.copyProperties(tempCartItem, cartItemResponseDTO);
		return cartItemResponseDTO;
	}

	public TempCartItem toEntity(final TempCartItemDTO tempCartItemDTO) {
		TempCartItem tempCartItem = new TempCartItem();
		BeanUtils.copyProperties(tempCartItemDTO, tempCartItem);
		return tempCartItem;
	}

	public List<CartItemResponseDTO> toDtos(final List<TempCartItem> tempCartItemList) {
		List<CartItemResponseDTO> results = new ArrayList<>();
		for (TempCartItem TempCartItem : tempCartItemList) {
			results.add(toDto(TempCartItem));
		}
		return results;
	}
}
