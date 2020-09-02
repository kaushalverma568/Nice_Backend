package com.nice.service;

import java.util.List;

import com.nice.dto.StateDTO;
import com.nice.dto.StateResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
public interface StateService {
	/**
	 * Add State for Country
	 *
	 * @param  stateDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	State addState(StateDTO stateDTO) throws ValidationException, NotFoundException;

	/**
	 * Update state for country
	 *
	 * @param  state
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	State updateState(StateDTO stateDTO) throws ValidationException, NotFoundException;

	/**
	 * Get State Details based on id
	 *
	 * @param  stateId
	 * @return
	 * @throws NotFoundException
	 */
	StateResponseDTO getState(final Long stateId) throws NotFoundException;

	/**
	 * to check state duplication and returning Boolean value.
	 *
	 * @param  stateDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isStateExistsEnglish(final StateDTO stateDTO) throws NotFoundException;

	/**
	 * to check state duplication and returning Boolean value.
	 *
	 * @param  stateDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isStateExistsArabic(final StateDTO stateDTO) throws NotFoundException;

	/**
	 * Change status of state (active/deActive)
	 *
	 * @param  stateId
	 * @param  active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long stateId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * Get State details based on Id : Specially for internally calls
	 *
	 * @param  stateId
	 * @return
	 * @throws NotFoundException
	 */
	State getStateDetails(Long stateId) throws NotFoundException;

	/**
	 * Get state list based on parameters
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 * @throws ValidationException
	 */
	List<State> getStateListBasedOnParams(Integer startIndex, Integer pageSize, Boolean activeRecords, Long countryId, String searchKeyword)
			throws ValidationException;

	/**
	 * Get state count based on parameters
	 *
	 * @param  activeRecords
	 * @param  countryId
	 * @param  searchKeyword
	 * @return
	 */
	Long getStateCountBasedOnParams(Boolean activeRecords, Long countryId, String searchKeyword);

}