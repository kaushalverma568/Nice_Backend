package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Repository
public interface UserLoginRepository extends JpaRepository<UserLogin, Long> {

	/**
	 * @param  email
	 * @return
	 */
	Optional<UserLogin> findByEmail(String email);

	/**
	 * @param  phoneNumber
	 * @param  entityType
	 * @return
	 */
	Optional<UserLogin> findByPhoneNumberAndEntityType(String phoneNumber, String entityType);

	/**
	 * @param entityId
	 * @param entityType
	 */
	Optional<UserLogin> findByEntityIdAndEntityType(Long entityId, String entityType);

	/**
	 * @param  email
	 * @param  id
	 * @return
	 */
	Optional<UserLogin> findByEmailAndIdNot(String email, Long id);

	/**
	 * @param  email
	 * @param  upperCase
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityType(String email, String entityType);

	/**
	 * @param  actualUser
	 * @param  name
	 * @return
	 */
	Optional<UserLogin> findByEmailAndRole(String actualUser, String name);

	/**
	 * @param  email
	 * @param  name
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityTypeIsNull(String email);

	/**
	 * get user login by email and entity type list or by email and entity type is null
	 *
	 * @param  userName
	 * @param  adminPanelUserList
	 * @param  userName2
	 * @return
	 */
	Optional<UserLogin> findByEmailAndEntityTypeInOrEmailAndEntityTypeIsNull(String userName, List<String> adminPanelUserList, String userName2);
}
