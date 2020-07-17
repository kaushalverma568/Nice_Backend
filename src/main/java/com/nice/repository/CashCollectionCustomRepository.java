package com.nice.repository;

import java.util.Date;
import java.util.List;

import com.nice.model.CashCollection;

public interface CashCollectionCustomRepository {


	Long getCountBasedOnParams(Long deliveryBoyId, Date createdAt);

	List<CashCollection> getListBasedOnParams(Integer startIndex, Integer pageSize, Long deliveryBoyId, Date createdAt);
	
}
