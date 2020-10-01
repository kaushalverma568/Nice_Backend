
package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.UserLogin;
import com.nice.model.UserOtp;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Jun-2020
 */
@Repository(value = "userOtpRepository")
public interface UserOtpRepository extends JpaRepository<UserOtp, Long> {

	Optional<UserOtp> findAllByTypeIgnoreCaseAndUserLoginAndOtpAndActive(String type, UserLogin userLogin, String otp, boolean active);

	/**
	 * @param userlogin
	 * @return
	 */
	UserOtp findByUserLogin(Optional<UserLogin> userlogin);

	/**
	 * @param userlogin
	 * @param type
	 * @return
	 */
	UserOtp findByUserLoginAndTypeIgnoreCase(Optional<UserLogin> userlogin, String type);

	/**
	 * Get user OTP by user id
	 *
	 * @param type
	 * @param userLogin
	 * @return
	 */
	Optional<UserOtp> findAllByTypeIgnoreCaseAndUserLogin(String type, UserLogin userLogin);

	/**
	 * get user otp by type userlogin and active
	 *
	 * @param type
	 * @param userLogin
	 * @param active
	 * @return
	 */
	Optional<UserOtp> findAllByTypeIgnoreCaseAndUserLoginAndActive(String type, UserLogin userLogin, boolean active);
}
