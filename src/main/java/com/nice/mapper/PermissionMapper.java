/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.PermissionDTO;
import com.nice.dto.PermissionResponseDTO;
import com.nice.model.Permission;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Component
public class PermissionMapper {

	public PermissionDTO toDTO(final Permission permission) {
		PermissionDTO permissionDTO = new PermissionDTO();
		BeanUtils.copyProperties(permission, permissionDTO);
		permissionDTO.setRoleId(permission.getRole().getId());
		permissionDTO.setModulesId(permission.getModules().getId());
		return permissionDTO;
	}

	public Permission toEntity(final PermissionDTO permissionDTO) {
		Permission permission = new Permission();
		BeanUtils.copyProperties(permissionDTO, permission);
		return permission;
	}

	public List<PermissionResponseDTO> toResponseDTOs(final List<Permission> permissions) {
		List<PermissionResponseDTO> permissionResponseDTOList = new ArrayList<>();
		for (Permission permission : permissions) {
			permissionResponseDTOList.add(toResponseDTO(permission));
		}
		return permissionResponseDTOList;
	}

	public PermissionResponseDTO toResponseDTO(final Permission permission) {
		PermissionResponseDTO permissionResponseDTO = new PermissionResponseDTO();
		BeanUtils.copyProperties(permission, permissionResponseDTO);
		permissionResponseDTO.setRoleId(permission.getRole().getId());
		permissionResponseDTO.setRoleName(permission.getRole().getName());
		permissionResponseDTO.setModuleId(permission.getModules().getId());
		permissionResponseDTO.setModuleName(permission.getModules().getName());
		return permissionResponseDTO;
	}
}
