package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.CashCollection;
import com.nice.model.Task;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Repository
public interface CashCollectionRepository extends JpaRepository<CashCollection, Long>, CashCollectionCustomRepository {
	/**
	 * get cash collection by task
	 *
	 * @param  task
	 * @return
	 */
	Optional<CashCollection> findByTask(Task task);
}
