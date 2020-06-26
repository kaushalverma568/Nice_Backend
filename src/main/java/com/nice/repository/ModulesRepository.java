package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Modules;

@Repository
public interface ModulesRepository extends JpaRepository<Modules, Long> {

	Modules findByName(String name);

	/**
	 * @param name
	 * @param id
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * @param name
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCase(String name);

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Modules> findAllByActive(Boolean activeRecords, Pageable pageable);

}
