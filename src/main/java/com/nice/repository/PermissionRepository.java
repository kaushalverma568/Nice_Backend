package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
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
	 * @param  role
	 * @param  module
	 * @param  id
	 * @return
	 */
	Optional<Permission> findByRoleAndModulesAndIdNot(Role role, Modules module, Long id);

	/**
	 * @param  role
	 * @param  module
	 * @return
	 */
	Optional<Permission> findByRoleAndModules(Role role, Modules module);

	@Query(value = "select * from permission p join role ro on ro.id=p.role_id join modules mo on p.modules_id=mo.id where ro.name=:role and mo.name=:moduleName", nativeQuery = true)
	Permission getRoleAndModuleWisePermission(@Param(value = "role") String role, @Param(value = "moduleName") String moduleName);

	/**
	 * get page by role,active and module
	 *
	 * @param  activeRecords
	 * @param  module
	 * @param  role
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActiveAndModulesAndRole(Boolean activeRecords, Modules module, Role role, Pageable pageable);

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
	 * get page by active and module
	 *
	 * @param  activeRecords
	 * @param  module
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByActiveAndModules(Boolean activeRecords, Modules module, Pageable pageable);

	/**
	 * get page by module
	 *
	 * @param  module
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByModules(Modules module, Pageable pageable);

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
	 * get page by role and module
	 *
	 * @param  role
	 * @param  module
	 * @param  pageable
	 * @return
	 */
	Page<Permission> findAllByRoleAndModules(Role role, Modules module, Pageable pageable);

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

}
