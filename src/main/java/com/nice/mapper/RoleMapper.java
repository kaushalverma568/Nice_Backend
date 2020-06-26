/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.RoleDTO;
import com.nice.model.Role;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class RoleMapper {

	public RoleDTO toDto(final Role role) {
		RoleDTO roleDTO = new RoleDTO();
		BeanUtils.copyProperties(role, roleDTO);
		return roleDTO;
	}

	public Role toEntity(final RoleDTO roleDTO) {
		Role role = new Role();
		BeanUtils.copyProperties(roleDTO, role);

		return role;
	}

	public List<RoleDTO> toDtos(final List<Role> roles) {
		List<RoleDTO> results = new ArrayList<>();
		for (Role c : roles) {
			results.add(toDto(c));
		}
		return results;
	}
}
