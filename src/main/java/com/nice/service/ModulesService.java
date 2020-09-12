package com.nice.service;

import org.springframework.data.domain.Page;

import com.nice.dto.ModulesDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Modules;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 26-06-2020
 */
public interface ModulesService {

	/**
	 * get module by id
	 *
	 * @param  moduleId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ModulesDTO getModule(Long moduleId) throws NotFoundException, ValidationException;

	/**
	 * Get module detail by id
	 *
	 * @param  moduleId
	 * @return
	 * @throws NotFoundException
	 */
	Modules getModuleDetail(Long moduleId) throws NotFoundException;

	/**
	 * get module by name if exist
	 *
	 * @param  name
	 * @return
	 * @throws NotFoundException
	 */
	Modules getModuleDetailByName(String name) throws NotFoundException;

	/**
	 * get module list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  availableForNewRole
	 * @return
	 */
	Page<Modules> getModuleList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Boolean availableForNewRole);

}
