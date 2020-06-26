package com.nice.service;

import java.util.List;

import com.nice.dto.PincodeDTO;
import com.nice.dto.PincodeResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Pincode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
public interface PincodeService {

	/**
	 * Add pincode for City
	 *
	 * @param  pincodeDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addPincode(PincodeDTO pincodeDTO) throws ValidationException, NotFoundException;

	/**
	 * Get pincode details based on pincodeId
	 *
	 * @param  pincodeId
	 * @return
	 * @throws NotFoundException
	 */
	PincodeResponseDTO getPincode(final Long pincodeId) throws NotFoundException;

	/**
	 * Get Pincode details based on Id : Specially for internally calls
	 *
	 * @param  pincodeId
	 * @return
	 * @throws NotFoundException
	 */
	Pincode getPincodeDetails(Long pincodeId) throws NotFoundException;

	/**
	 * Check pincode is exists for same city other than the one getting updated now
	 *
	 * @param  pincodeDTO
	 * @return
	 * @throws NotFoundException
	 */
	Boolean isPincodeExists(PincodeDTO pincodeDTO);

	/**
	 * Change status of pincode (active/deActive)
	 *
	 * @param  pincodeId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long pincodeId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * Get pincode count based on parameters
	 *
	 * @param  activeRecords
	 * @param  cityId
	 * @param  searchKeyword
	 * @return
	 */
	Long getPincodeCountBasedOnParams(Boolean activeRecords, Long cityId, String searchKeyword);

	/**
	 * Get pincode list based on parameters
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  cityId
	 * @param  searchKeyword
	 * @return
	 */
	List<Pincode> getPincodeListBasedOnParams(Integer startIndex, Integer pageSize, Boolean activeRecords, Long cityId, String searchKeyword);

}
