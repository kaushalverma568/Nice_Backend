package com.nice.service;

import java.util.Date;
import java.util.List;

import com.nice.dto.CashCollectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.model.CashCollection;

public interface CashcollectionService {

	
	/**
	 * 
	 * @param cashCollectionDto
	 * @return
	 * @throws NotFoundException
	 */
	public CashCollectionDTO addCashCollection( CashCollectionDTO cashCollectionDto) throws NotFoundException ;

	
	/**
	 * 
	 * @param cashCollectionId
	 * @return
	 * @throws NotFoundException
	 */
	public CashCollectionDTO getCashCollection(Long cashCollectionId) throws NotFoundException;

	/**
	 * 
	 * @param cashCollectionId
	 * @return
	 * @throws NotFoundException
	 */
	CashCollection getCashCollectionDetail(Long cashCollectionId) throws NotFoundException;

	/**
	 * 
	 * @param deliveryBoyId
	 * @param paidDate
	 * @return
	 */
	public Long getCountBasedOnParams(Long deliveryBoyId, Date createdAt);

    /**
     * 
     * @param startIndex
     * @param pageSize
     * @param deliveryBoyId
     * @param paidDate
     * @return
     */
	public List<CashCollection> getListBasedOnParams(Integer startIndex, Integer pageSize, Long deliveryBoyId,
			Date createdAt);
	
}
