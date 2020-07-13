package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Users;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface UsersRepository extends JpaRepository<Users, Long> {

	/**
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<Users> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * find by email and id not
	 *
	 * @param email
	 * @param id
	 * @return
	 */
	Optional<Users> findByEmailAndIdNot(String email, Long id);

	/**
	 * find by email
	 *
	 * @param email
	 * @return
	 */
	Optional<Users> findByEmail(String email);

}
