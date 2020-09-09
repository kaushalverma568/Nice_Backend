package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.WalletTrxDTO;
import com.nice.model.WalletTrx;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
@Component
public class WalletTrxMapper {

	public WalletTrxDTO toDto(final WalletTrx walletTrx) {
		WalletTrxDTO walletTrxDTO = new WalletTrxDTO();
		BeanUtils.copyProperties(walletTrx, walletTrxDTO);
		walletTrxDTO.setFirstName(walletTrx.getCustomer().getFirstName());
		walletTrxDTO.setLastName(walletTrx.getCustomer().getLastName());
		walletTrxDTO.setCustomerId(walletTrx.getCustomer().getId());
		walletTrxDTO.setOrderId(walletTrx.getOrder().getId());
		return walletTrxDTO;
	}

	public WalletTrx toEntity(final WalletTrxDTO walletTrxDTO) {
		WalletTrx walletTrx = new WalletTrx();
		BeanUtils.copyProperties(walletTrxDTO, walletTrx);
		return walletTrx;
	}

	public List<WalletTrxDTO> toDtos(final List<WalletTrx> countries) {
		List<WalletTrxDTO> results = new ArrayList<>();
		for (WalletTrx c : countries) {
			results.add(toDto(c));
		}
		return results;
	}
}
