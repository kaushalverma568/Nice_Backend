package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.UsersDTO;
import com.nice.dto.UsersResponseDTO;
import com.nice.model.Users;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Component
public class UsersMapper {

	public UsersResponseDTO toDto(final Users users) {
		Locale locale = LocaleContextHolder.getLocale();
		UsersResponseDTO usersResponseDTO = new UsersResponseDTO();
		BeanUtils.copyProperties(users, usersResponseDTO);
		usersResponseDTO.setRoleId(users.getRole().getId());
		usersResponseDTO.setRoleName(users.getRole().getName());
		if (locale.getLanguage().equals("en")) {
			usersResponseDTO.setFirstName(users.getFirstNameEnglish());
			usersResponseDTO.setLastName(users.getLastNameEnglish());
		} else {
			usersResponseDTO.setFirstName(users.getFirstNameArabic());
			usersResponseDTO.setLastName(users.getLastNameArabic());
		}
		return usersResponseDTO;
	}

	public Users toEntity(final UsersDTO usersDTO) {
		Users users = new Users();
		BeanUtils.copyProperties(usersDTO, users);
		users.setEmail(usersDTO.getEmail().toLowerCase());
		return users;
	}

	public List<UsersResponseDTO> toDtos(final List<Users> usersList) {
		List<UsersResponseDTO> results = new ArrayList<>();
		for (Users users : usersList) {
			results.add(toDto(users));
		}
		return results;
	}
}