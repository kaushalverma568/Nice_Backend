package com.nice.service;

import java.util.Optional;

import com.nice.exception.NotFoundException;
import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
public interface UserLoginService {

	/**
	 * Add user login
	 *
	 * @param  userLogin
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin addUserLogin(UserLogin userLogin, Long userId) throws NotFoundException;

	/**
	 * Update user login
	 *
	 * @param  userLogin
	 * @return
	 * @throws NotFoundException
	 */
	UserLogin updateUserLogin(UserLogin userLogin) throws NotFoundException;

	/**
	 * @param  email
	 * @return
	 */
	Optional<UserLogin> getUserLoginBasedOnEmail(String email);

	/**
	 * @param  userLoginId
	 * @return
	 */
	Optional<UserLogin> getUserLogin(Long userLoginId);

}
