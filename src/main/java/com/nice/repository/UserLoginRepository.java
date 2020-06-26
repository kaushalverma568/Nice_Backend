package com.nice.repository;

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

	Optional<UserLogin> findByEmail(String email);

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
}
