package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.RoleAndPermissionResponseDTO;
import com.nice.dto.RoleAndPermissionsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Role;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
public interface RoleService {

	/**
	 * add/update role with permissions
	 *
	 * @param  roleAndPermissinsDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addUpdateRoleWithPermissions(RoleAndPermissionsDTO roleAndPermissinsDTO) throws ValidationException, NotFoundException;

	/**
	 * get role list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  isDefault
	 * @return
	 */
	Page<Role> getRoleList(Integer pageNumber, Integer pageSize, Boolean activeRecords, final Boolean isDefault);

	/**
	 * is role exist for name
	 *
	 * @param  roleDto
	 * @return
	 */
	boolean isExists(RoleAndPermissionsDTO roleAndPermissionsDTO);

	/**
	 * get role detail object by id
	 *
	 * @param  roleId
	 * @return
	 * @throws NotFoundException
	 */
	Role getRoleDetail(Long roleId) throws NotFoundException;

	/**
	 * delete role
	 *
	 * @param  roleId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteRole(Long roleId) throws NotFoundException, ValidationException;

	/**
	 * @param  roleId
	 * @return
	 * @throws NotFoundException
	 */
	RoleAndPermissionResponseDTO getRoleDetailWithPermission(Long roleId) throws NotFoundException;

	/**
	 * Get role detail name id
	 *
	 * @param  name
	 * @return
	 * @throws NotFoundException
	 */
	Role getRoleDetailByName(String name) throws NotFoundException;
}
