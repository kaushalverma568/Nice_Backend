package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.CartItemDTO;
import com.nice.dto.CartItemResponseDTO;
import com.nice.model.CartItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@Component
public class CartItemMapper {

	public CartItemResponseDTO toDto(final CartItem cartItem) {
		CartItemResponseDTO cartItemResponseDTO = new CartItemResponseDTO();
		BeanUtils.copyProperties(cartItem, cartItemResponseDTO);
		return cartItemResponseDTO;
	}

	public CartItem toEntity(final CartItemDTO cartItemDTO) {
		CartItem cartItem = new CartItem();
		BeanUtils.copyProperties(cartItemDTO, cartItem);
		return cartItem;
	}

	public List<CartItemResponseDTO> toDtos(final List<CartItem> cartItemList) {
		List<CartItemResponseDTO> results = new ArrayList<>();
		for (CartItem CartItem : cartItemList) {
			results.add(toDto(CartItem));
		}
		return results;
	}
}
