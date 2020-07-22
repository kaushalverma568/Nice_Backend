package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Repository
public interface UserLoginRepository extends JpaRepository<UserLogin, Long> {

	/**
	 * @param phoneNumber
	 * @param entityType
	 * @return
	 */
	Optional<UserLogin> findByPhoneNumberIgnoreCaseAndEntityType(String phoneNumber, String entityType);

	/**
	 * @param entityId
	 * @param entityType
	 */
	Optional<UserLogin> findByEntityIdAndEntityType(Long entityId, String entityType);

	/**
	 * @param email
	 * @param upperCase
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityType(String email, String entityType);

	/**
	 * @param actualUser
	 * @param name
	 * @return
	 */
	Optional<UserLogin> findByEmailAndRole(String actualUser, String name);

	/**
	 * @param email
	 * @param name
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityTypeIsNull(String email);

	/**
	 * @param userName
	 * @param entityType
	 * @param userName1
	 * @param entityType1
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityTypeOrPhoneNumberIgnoreCaseAndEntityType(String userName, String entityType, String userName1, String entityType1);

	/**
	 * get admin panel user's login detail based entity type(null or admin panel
	 * user's user Type) and user name(email or phone number)
	 *
	 * @param userName
	 * @param adminPanelUserList
	 * @param userName2
	 * @return
	 */
	@Query("select u from UserLogin u where ( u.entityType IS NULL or u.entityType in :adminPanelUserList ) and ( u.email=:userName or u.phoneNumber=:userName ) ")
	Optional<UserLogin> getAdminPanelUserBasedOnUserNameAndEntityType(String userName, List<String> adminPanelUserList);

	/**
	 * get user login details based on email and entity type : this method is used
	 * at the time of update super admin's email
	 *
	 * @param email
	 * @param adminPanelUserList
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityTypeIn(String email, List<String> adminPanelUserList);
}
