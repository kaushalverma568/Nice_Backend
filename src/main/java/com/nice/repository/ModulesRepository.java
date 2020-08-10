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
	 * get modules by name,userRole and id not
	 *
	 * @param  name
	 * @param  userRole
	 * @param  id
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCaseAndUserRoleAndIdNot(String name, String userRole, Long id);

	/**
	 * get modules by name,userRole
	 *
	 * @param  name
	 * @param  userRole
	 * @return
	 */
	Optional<Modules> findByNameIgnoreCaseAndUserRole(String name, String userRole);

	/**
	 * @param  activeRecords
	 * @param  userRole
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByActiveAndUserRole(Boolean activeRecords, String userRole, Pageable pageable);

	/**
	 * @param  userRole
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByUserRole(String userRole, Pageable pageable);

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Modules> findAllByActive(Boolean activeRecords, Pageable pageable);

}
