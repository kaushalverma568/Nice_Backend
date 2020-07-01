package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.StockTransfer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 31-Jan-2020
 */
@Repository("internalStockTransferRepository")
public interface StockTransferRepository extends JpaRepository<StockTransfer, Long> {

}
