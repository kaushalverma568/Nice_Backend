package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.UsersDTO;
import com.nice.dto.UsersResponseDTO;
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
	void addUsers(UsersDTO usersDTO) throws NotFoundException, ValidationException;

	/**
	 * Update users
	 *
	 * @param  usersDTO
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateUsers(UsersDTO usersDTO) throws ValidationException, NotFoundException;

	/**
	 * Get users details
	 *
	 * @param  usersId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	UsersResponseDTO getUsers(Long usersId) throws NotFoundException, ValidationException;

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

	/**
	 *
	 * @param  usersId
	 * @return
	 * @throws NotFoundException
	 */
	Users getUsersDetails(final Long usersId) throws NotFoundException;

	/**
	 * Check is super admin contains same email address
	 *
	 * @param  usersDTO
	 * @return
	 */
	boolean isSuperAdminExists(UsersDTO usersDTO);

	/**
	 * Check is vendor contains same email address
	 *
	 * @param  usersDTO
	 * @return
	 */
	boolean isVendorExists(UsersDTO usersDTO);
}
