package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.AddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Addons;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 14-Jul-2020
 */
public interface AddonsService {
	/**
	 * check is exists or not
	 *
	 * @param  addonsDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Boolean isExists(AddonsDTO addonsDto) throws NotFoundException, ValidationException;

	/**
	 * add addons
	 *
	 * @param  addonsDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addAddons(AddonsDTO addonsDTO) throws NotFoundException, ValidationException;

	/**
	 * update addons
	 *
	 * @param  addonsDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateAddons(AddonsDTO addonsDTO) throws NotFoundException, ValidationException;

	/**
	 * get addons detail in DTO by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	AddonsDTO getAddonsDetailById(Long id) throws NotFoundException;

	/**
	 * get Addons by id
	 *
	 * @param  id
	 * @return
	 * @throws NotFoundException
	 */
	Addons getAddonsById(Long id) throws NotFoundException;

	/**
	 * paginated and filter addons list based on params
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	Page<Addons> getAddonsList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword, Long vendorId) throws NotFoundException;

	/**
	 * change status (active/deactive)
	 *
	 * @param  addonsId
	 * @param  active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long addonsId, Boolean active) throws NotFoundException, ValidationException;

}
