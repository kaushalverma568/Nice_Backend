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
	 * add module
	 *
	 * @param  moduleDto
	 * @throws ValidationException
	 */
	void addModule(ModulesDTO moduleDto) throws ValidationException;

	/**
	 * update module
	 *
	 * @param  moduleDto
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateModule(ModulesDTO moduleDto) throws NotFoundException, ValidationException;

	/**
	 * get module
	 *
	 * @param  moduleId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	ModulesDTO getModule(Long moduleId) throws NotFoundException, ValidationException;

	/**
	 * change status of module
	 *
	 * @param  permissionId
	 * @param  isActive
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long moduleId, Boolean isActive) throws ValidationException, NotFoundException;

	/**
	 * is module exist or not
	 *
	 * @param  modulesDto
	 * @return
	 */
	boolean isExists(ModulesDTO modulesDto);

	/**
	 * get module list
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @return
	 */
	Page<Modules> getModuleList(Integer pageNumber, Integer pageSize, Boolean activeRecords);

	/**
	 * @param  moduleId
	 * @return
	 * @throws NotFoundException
	 */
	Modules getModuleDetail(Long moduleId) throws NotFoundException;

}
