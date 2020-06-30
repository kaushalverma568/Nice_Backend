package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.nice.model.SchedulerDetails;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 30-Jun-2020
 */
public interface SchedulerDetailsRepository extends JpaRepository<SchedulerDetails, Long> {

	/**
	 * @param  name
	 * @return
	 */
	Optional<SchedulerDetails> findByName(String name);

}
