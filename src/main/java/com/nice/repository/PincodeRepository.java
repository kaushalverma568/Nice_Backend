package com.nice.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.City;
import com.nice.model.Pincode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
@Repository
public interface PincodeRepository extends JpaRepository<Pincode, Long>, PincodeCustomRepository {

	/**
	 * Get Page based on active field
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Pincode> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get Pincode based on codeValue for not given id
	 *
	 * @param  codeValue
	 * @param  city
	 * @param  id
	 * @return
	 */
	Optional<Pincode> findByCodeValueIgnoreCaseAndCityAndIdNot(String codeValue, City city, Long id);

	/**
	 * Get Pincode based on codeValue
	 *
	 * @param  codeValue
	 * @param  city
	 * @return
	 */
	Optional<Pincode> findByCodeValueIgnoreCaseAndCity(String codeValue, City city);

}
