package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.WalletTrxDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.WalletTrx;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 09-Sep-2020
 */
public interface WalletTrxService {

	void addupdateWalletTrx(WalletTrxDTO walletTrxDTO) throws NotFoundException;

	Page<WalletTrx> getWalletTrxList(Integer pageNumber, Integer pageSize, Long customerId) throws NotFoundException;

}
