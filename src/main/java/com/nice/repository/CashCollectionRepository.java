package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CashCollection;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface CashCollectionRepository extends JpaRepository<CashCollection, Long>, CashCollectionCustomRepository {
	

}
