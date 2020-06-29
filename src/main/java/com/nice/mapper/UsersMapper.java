package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.UsersDTO;
import com.nice.model.Users;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class UsersMapper {

	public UsersDTO toDto(final Users users) {
		UsersDTO usersDTO = new UsersDTO();
		BeanUtils.copyProperties(users, usersDTO);
		return usersDTO;
	}

	public Users toEntity(final UsersDTO usersDTO) {
		Users users = new Users();
		BeanUtils.copyProperties(usersDTO, users);
		return users;
	}

	public List<UsersDTO> toDtos(final List<Users> usersList) {
		List<UsersDTO> results = new ArrayList<>();
		for (Users users : usersList) {
			results.add(toDto(users));
		}
		return results;
	}
}