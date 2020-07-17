package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.SettingHistory;



/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface SettingHistoryRepository extends JpaRepository<SettingHistory, Long>, SettingHistoryCustomRepository {

	
	

}
