package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Modules;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface ModulesRepository extends JpaRepository<Modules, Long> {

	/**
	 * get modules by name
	 *
	 * @param  name
	 * @return
	 */
	Optional<Modules> findByName(String name);

	/**
	 * get modules by name and id not
	 *
	 * @param  name
	 * @param  id
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * get modules by name
	 *
	 * @param  name
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCase(String name);

	/**
	 * get modules by active
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * get modules by active and availableForNewRole
	 *
	 * @param  activeRecords
	 * @param  availableForNewRole
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByActiveAndAvailableForNewRole(Boolean activeRecords, Boolean availableForNewRole, Pageable pageable);

	/**
	 * get modules by availableForNewRole
	 *
	 * @param  availableForNewRole
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByAvailableForNewRole(Boolean availableForNewRole, Pageable pageable);

}
