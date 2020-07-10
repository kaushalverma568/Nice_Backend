package com.nice.service;

import java.util.List;

import com.nice.dto.VendorCuisineDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.VendorCuisine;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
public interface VendorCuisineService {
	/**
	 * add update vendor cuisine
	 *
	 * @param vendorCuisineDTO
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addUpdateVendorCuisine(VendorCuisineDTO vendorCuisineDTO) throws ValidationException, NotFoundException;

	/**
	 * check for vendor+cuisin already exists or not
	 *
	 * @param vendorCuisineDTO
	 * @return
	 */
	boolean isAlreadyExist(VendorCuisineDTO vendorCuisineDTO);

	/**
	 * change status of all vendor cuisine by vendor id
	 *
	 * @param vendorId
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void bulkChangeAllStatus(List<VendorCuisine> vendorCuisines, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * get all vendor cuisine list by vendor id and active
	 *
	 * @param vendorId
	 * @param active
	 * @return
	 */
	List<VendorCuisine> getVendorCuisineListByVendor(Long vendorId, Boolean active);

	/**
	 * get vendor cuisine by id
	 *
	 * @param id
	 * @return
	 * @throws NotFoundException
	 */
	VendorCuisine getVendorCuisineById(Long id) throws NotFoundException;

	/**
	 * change status of vendor cuisine by id
	 *
	 * @param id
	 * @param active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long id, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * get dto list by vendor and active
	 *
	 * @param vendorId
	 * @param active
	 * @return
	 */
	List<VendorCuisineDTO> getVendorCuisineDetailListByVendor(Long vendorId, Boolean active);

	/**
	 * @param vendorId
	 * @param cuisineId
	 * @return
	 * @throws NotFoundException
	 */
	VendorCuisine getVendorCuisineByVendorIdAndCuisineId(Long vendorId, Long cuisineId) throws NotFoundException;

	/**
	 * get all vendor cuisine list by cuisine id and active
	 *
	 * @param cuisineId
	 * @param active
	 * @return
	 * @throws NotFoundException
	 */
	List<VendorCuisine> getVendorCuisineListByCuisine(Long cuisineId, Boolean active) throws NotFoundException;
}
