package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.CountryDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Country;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
public interface CountryService {
	/**
	 * Add country
	 *
	 * @param  countryDTO
	 * @return
	 * @throws ValidationException
	 */
	Country addCountry(CountryDTO countryDTO) throws ValidationException;

	/**
	 * Update country
	 *
	 * @param  country
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Country updateCountry(CountryDTO countryDTO) throws NotFoundException, ValidationException;

	/**
	 * Get details of country
	 *
	 * @param  countryId
	 * @return
	 * @throws NotFoundException
	 */
	CountryDTO getCountry(Long countryId) throws NotFoundException;

	/**
	 * Change status of country (active/deActive)
	 *
	 * @param  countryId
	 * @param  active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long countryId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * to check country duplication and returning Boolean value.
	 *
	 * @param  country
	 * @return
	 * @throws ValidationException
	 */
	boolean isCountryExists(CountryDTO countryDTO);

	/**
	 * Get count of country
	 *
	 * @return
	 */
	Long getCountOfCountry();

	/**
	 * Get List of country based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<Country> getCountryList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord)
			throws NotFoundException, ValidationException;

	/**
	 * Get Country details based on Id : Specially for internally calls
	 *
	 * @param  countryId
	 * @return
	 * @throws NotFoundException
	 */
	Country getCountryDetails(Long countryId) throws NotFoundException;

}
