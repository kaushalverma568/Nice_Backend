package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Modules;
import com.nice.model.Permission;
import com.nice.model.Role;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
@Repository
public interface PermissionRepository extends JpaRepository<Permission, Long> {

	/**
	 * get all permission by active
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * get permission by role,modules and id not if exist
	 *
	 * @param  role
	 * @param  modules
	 * @param  id
	 * @return
	 */
	Optional<Permission> findByRoleAndModulesAndIdNot(Role role, Modules modules, Long id);

	/**
	 * get permission by role and modules if exist
	 *
	 * @param  role
	 * @param  modules
	 * @return
	 */
	Optional<Permission> findByRoleAndModules(Role role, Modules modules);

	/**
	 * get page by role,active and modules
	 *
	 * @param  activeRecords
	 * @param  modules
	 * @param  role
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActiveAndModulesAndRole(Boolean activeRecords, Modules modules, Role role, Pageable pageable);

	/**
	 * get page by role and active
	 *
	 * @param  activeRecords
	 * @param  role
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActiveAndRole(Boolean activeRecords, Role role, Pageable pageable);

	/**
	 * get permission list by active and role
	 *
	 * @param  activeRecords
	 * @param  role
	 * @return
	 */
	List<Permission> findAllByActiveAndRole(Boolean activeRecords, Role role);

	/**
	 * get page by active and modules
	 *
	 * @param  activeRecords
	 * @param  modules
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActiveAndModules(Boolean activeRecords, Modules modules, Pageable pageable);

	/**
	 * get page by modules
	 *
	 * @param  modules
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByModules(Modules modules, Pageable pageable);

	/**
	 * get page by role
	 *
	 * @param  role
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByRole(Role role, Pageable pageable);

	/**
	 * get permission list by role
	 *
	 * @param  role
	 * @return
	 */
	List<Permission> findAllByRole(Role role);

	/**
	 * get page by role and modules
	 *
	 * @param  role
	 * @param  modules
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByRoleAndModules(Role role, Modules modules, Pageable pageable);

	/**
	 * get all by role,active and module
	 *
	 * @param  role
	 * @param  modules
	 * @param  active
	 * @return
	 */
	List<Permission> findAllByRoleAndModulesAndActive(Role role, Modules modules, Boolean active);

	/**
	 * get all by role and active
	 *
	 * @param  role
	 * @param  active
	 * @return
	 */
	List<Permission> findAllByRoleAndActive(Role role, Boolean active);

	/**
	 * get all by active and module
	 *
	 * @param  modules
	 * @param  active
	 * @return
	 */
	List<Permission> findAllByModulesAndActive(Modules modules, Boolean active);

	/**
	 * get all by active
	 *
	 * @param  active
	 * @return
	 */
	List<Permission> findAllByActive(Boolean active);

	/**
	 * Get permission list by role,canView and active
	 *
	 * @param  role
	 * @param  canView
	 * @param  active
	 * @return
	 */
	List<Permission> findAllByRoleAndCanViewAndActiveOrderByModulesAsc(Role role, Boolean canView, Boolean active);

}
