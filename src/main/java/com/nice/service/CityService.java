package com.nice.service;

import java.util.List;

import com.nice.dto.CityDTO;
import com.nice.dto.CityResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.City;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
public interface CityService {

	/**
	 * Add city for State
	 *
	 * @param cityDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addCity(CityDTO cityDTO) throws ValidationException, NotFoundException;

	/**
	 * Update city for state
	 *
	 * @param cityDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateCity(CityDTO cityDTO) throws ValidationException, NotFoundException;

	/**
	 * Get city details based on cityId
	 *
	 * @param cityId
	 * @return
	 * @throws NotFoundException
	 */
	CityResponseDTO getCity(final Long cityId) throws NotFoundException;

	/**
	 * Get City details based on Id : Specially for internally calls
	 *
	 * @param cityId
	 * @return
	 * @throws NotFoundException
	 */
	City getCityDetails(Long cityId) throws NotFoundException;

	/**
	 * Check city is exists for same state other than the one getting updated now
	 *
	 * @param cityDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isCityExistsEnglish(CityDTO cityDTO) throws NotFoundException;

	/**
	 * Change status of city (active/deActive)
	 *
	 * @param cityId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long cityId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * Get city count based on parameters
	 *
	 * @param activeRecords
	 * @param stateId
	 * @param searchKeyword
	 * @return
	 */
	Long getCityCountBasedOnParams(Boolean activeRecords, Long stateId, String searchKeyword);

	/**
	 * Get city list based on parameters
	 *
	 * @param startIndex
	 * @param pageSize
	 * @param activeRecords
	 * @param stateId
	 * @param searchKeyword
	 * @return
	 */
	List<City> getCityListBasedOnParams(Integer startIndex, Integer pageSize, Boolean activeRecords, Long stateId, String searchKeyword);

	/**
	 * @param cityDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isCityExistsArabic(CityDTO cityDTO) throws NotFoundException;

}
