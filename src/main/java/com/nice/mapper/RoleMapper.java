/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.dto.RoleAndPermissionsDTO;
import com.nice.dto.RoleResponseDTO;
import com.nice.model.Role;
import com.nice.model.UserLogin;
import com.nice.service.UserLoginService;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
@Component
public class RoleMapper {

	@Autowired
	private UserLoginService userLoginService;

	public RoleResponseDTO toDto(final Role role) {
		RoleResponseDTO roleDTO = new RoleResponseDTO();
		BeanUtils.copyProperties(role, roleDTO);
		Optional<UserLogin> createdUser = userLoginService.getUserLogin(role.getCreatedBy());
		if (createdUser.isPresent()) {
			roleDTO.setCreatedBy(CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(createdUser.get().getEntityType()) ? createdUser.get().getEntityType() : "Admin");
		}
		Optional<UserLogin> updatedUser = userLoginService.getUserLogin(role.getUpdatedBy());
		if (updatedUser.isPresent()) {
			roleDTO.setUpdatedBy(CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(updatedUser.get().getEntityType()) ? updatedUser.get().getEntityType() : "Admin");
		}
		return roleDTO;
	}

	public Role toEntity(final RoleAndPermissionsDTO roleAndPermissionsDTO) {
		Role role = new Role();
		BeanUtils.copyProperties(roleAndPermissionsDTO, role);
		return role;
	}

	public List<RoleResponseDTO> toDtos(final List<Role> roles) {
		List<RoleResponseDTO> results = new ArrayList<>();
		for (Role c : roles) {
			results.add(toDto(c));
		}
		return results;
	}
}
