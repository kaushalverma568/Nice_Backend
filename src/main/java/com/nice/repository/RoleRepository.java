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
	/**
	 * Get role by name if exist
	 *
	 * @param  name
	 * @return
	 */
	Optional<Role> findByName(String name);

	/**
	 * Get role by name ignore case if exist
	 *
	 * @param  name
	 * @return
	 */
	Optional<Role> findByNameIgnoreCase(String name);

	/**
	 * Get role by name ignore case and id not if exist
	 *
	 * @param  roleName
	 * @param  id
	 * @return
	 */
	Optional<Role> findByNameIgnoreCaseAndIdNot(String name, Long id);

	/**
	 * Get page of role by active
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Role> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get page of role by active and isDefault
	 *
	 * @param  activeRecords
	 * @param  isDefault
	 * @param  pageable
	 * @return
	 */
	Page<Role> findAllByActiveAndIsDefault(Boolean activeRecords, Boolean isDefault, Pageable pageable);

	/**
	 * Get page of role by isDefault
	 *
	 * @param  isDefault
	 * @param  pageable
	 * @return
	 */
	Page<Role> findAllByIsDefault(Boolean isDefault, Pageable pageable);

}
