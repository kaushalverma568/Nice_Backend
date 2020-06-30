package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.UsersDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Users;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
public interface UsersService {

	/**
	 * Add users
	 *
	 * @param  usersDTO
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Users addUsers(UsersDTO usersDTO) throws NotFoundException, ValidationException;

	/**
	 * Update users
	 *
	 * @param  usersDTO
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Users updateUsers(UsersDTO usersDTO) throws ValidationException, NotFoundException;

	/**
	 * Get users details
	 *
	 * @param  usersId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	UsersDTO getUsers(Long usersId) throws NotFoundException, ValidationException;

	/**
	 * Get users list based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	Page<Users> getUsersList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * Change status of users (active/deActive)
	 *
	 * @param  usersId
	 * @param  active
	 * @param  userId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long usersId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  usersDTO
	 * @return
	 */
	boolean isUserExists(UsersDTO usersDTO);

}
