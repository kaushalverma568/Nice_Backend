package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
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
		Locale locale = LocaleContextHolder.getLocale();
		UsersDTO usersDTO = new UsersDTO();
		BeanUtils.copyProperties(users, usersDTO);
		if (locale.getLanguage().equals("en")) {
			usersDTO.setFirstName(users.getFirstNameEnglish());
			usersDTO.setLastName(users.getLastNameEnglish());
		} else {
			usersDTO.setFirstName(users.getFirstNameArabic());
			usersDTO.setLastName(users.getLastNameArabic());
		}
		return usersDTO;
	}

	public Users toEntity(final UsersDTO usersDTO) {
		Users users = new Users();
		BeanUtils.copyProperties(usersDTO, users);
		users.setEmail(usersDTO.getEmail().toLowerCase());
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