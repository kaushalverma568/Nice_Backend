package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Role;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface RoleRepository extends JpaRepository<Role, Long> {

	Role findByName(String name);

	/**
	 * @param  name
	 * @return
	 */
	Optional<Role> findByNameIgnoreCase(String name);

	/**
	 * @param  roleName
	 * @param  id
	 * @return
	 */
	Optional<Role> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Role> findAllByActive(Boolean activeRecords, Pageable pageable);

}
