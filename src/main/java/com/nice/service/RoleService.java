package com.nice.service;

import javax.validation.Valid;

import org.springframework.data.domain.Page;

import com.nice.dto.RoleAndPermissionsDTO;
import com.nice.dto.RoleDTO;
import com.nice.dto.RoleResponseDTO;
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
	 * add role
	 *
	 * @param roleDTO
	 */
	void addRole(RoleDTO roleDTO);

	/**
	 * update role
	 *
	 * @param  roleDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateRole(RoleDTO roleDto) throws NotFoundException, ValidationException;

	/**
	 * get role by id
	 *
	 * @param  roleId
	 * @return
	 * @throws NotFoundException
	 */
	RoleResponseDTO getRole(Long roleId) throws NotFoundException;

	/**
	 * get role list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	Page<Role> getRoleList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * change status of role
	 *
	 * @param  roleId
	 * @param  isActive
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long roleId, Boolean isActive) throws ValidationException, NotFoundException;

	/**
	 * is role exist for name
	 *
	 * @param  roleDto
	 * @return
	 */
	boolean isExists(RoleDTO roleDTO);

	/**
	 * get role detail object ny id
	 *
	 * @param  roleId
	 * @return
	 * @throws NotFoundException
	 */
	Role getRoleDetail(Long roleId) throws NotFoundException;

	/**
	 * add/update role with permissions
	 *
	 * @param  roleAndPermissinsDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addUpdateRoleWithPermissions(@Valid RoleAndPermissionsDTO roleAndPermissinsDTO) throws ValidationException, NotFoundException;

	/**
	 * delete role
	 *
	 * @param  roleId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteRole(Long roleId) throws NotFoundException, ValidationException;
}
