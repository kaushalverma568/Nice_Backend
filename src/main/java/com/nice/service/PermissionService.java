package com.nice.service;

import java.util.List;
import java.util.Map;

import org.springframework.data.domain.Page;

import com.nice.dto.PermissionDTO;
import com.nice.dto.PermissionResponseDTO;
import com.nice.dto.SideBarDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Modules;
import com.nice.model.Permission;
import com.nice.model.Role;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
public interface PermissionService {
	/**
	 * add permission
	 *
	 * @param  permissionDTO
	 * @throws NotFoundException
	 */
	void addPermission(PermissionDTO permissionDTO) throws NotFoundException;

	/**
	 * update permission
	 *
	 * @param  permissionDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updatePermission(PermissionDTO permissionDTO) throws NotFoundException, ValidationException;

	/**
	 * get permission by id
	 *
	 * @param  permissionId
	 * @return
	 * @throws NotFoundException
	 */
	PermissionResponseDTO getPermission(Long permissionId) throws NotFoundException;

	/**
	 * get permission list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 */
	Page<Permission> getPermissionList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Long roleId, Long moduleId) throws NotFoundException;

	/**
	 * change status
	 *
	 * @param  permissionId
	 * @param  isActive
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long permissionId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * validate is permission exist for this role and modules
	 *
	 * @param  permissionDTO
	 * @param  role
	 * @param  modules
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	boolean isExists(PermissionDTO permissionDTO, Modules modules, Role role);

	/**
	 * get role and module wise permission map
	 *
	 * @param  role
	 * @param  moduleName
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Map<String, Boolean> getRoleAndModuleWisePermission(Role role, String moduleName) throws ValidationException, NotFoundException;

	/**
	 * get permission detail
	 *
	 * @param  permissionId
	 * @return
	 * @throws NotFoundException
	 */
	Permission getPermissionDetail(Long permissionId) throws NotFoundException;

	/**
	 * get permission list by role or module and active
	 *
	 * @param  roleId
	 * @param  existingModules
	 * @param  true1
	 * @throws ValidationException
	 */
	List<Permission> getPermissionList(Role role, Modules existingModules, Boolean true1) throws ValidationException;

	/**
	 * Get side bar specific permission list for user
	 *
	 * @return
	 * @throws NotFoundException
	 */
	List<SideBarDTO> getSideBarSpectificPermissionListForUser() throws NotFoundException;

}
