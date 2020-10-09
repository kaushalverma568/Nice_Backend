package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;

import com.nice.dto.AreaDTO;
import com.nice.dto.AreaResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Area;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
public interface AreaService {
	/**
	 * Add area
	 *
	 * @param  areaDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addArea(AreaDTO areaDTO) throws ValidationException, NotFoundException;

	/**
	 * Update area
	 *
	 * @param  area
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateArea(AreaDTO areaDTO) throws NotFoundException, ValidationException;

	/**
	 * Get details of area
	 *
	 * @param  areaId
	 * @return
	 * @throws NotFoundException
	 */
	AreaResponseDTO getArea(Long areaId) throws NotFoundException;

	/**
	 * Change status of area (active/deActive)
	 *
	 * @param  areaId
	 * @param  active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatus(Long areaId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * to check area duplication and returning Boolean value.
	 *
	 * @param  area
	 * @return
	 * @throws NotFoundException
	 */
	boolean isAreaExistsEnglish(AreaDTO areaDTO) throws NotFoundException;

	/**
	 * To check area duplication of arabic name and returning Boolean value.
	 *
	 * @param  areaDTO
	 * @return
	 * @throws NotFoundException
	 */
	boolean isAreaExistsArabic(AreaDTO areaDTO) throws NotFoundException;

	/**
	 * Get List of area based on parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @param  cityId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Page<Area> getAreaList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyWord, Long cityId)
			throws NotFoundException, ValidationException;

	/**
	 * Get Area details based on Id : Specially for internally calls
	 *
	 * @param  areaId
	 * @return
	 * @throws NotFoundException
	 */
	Area getAreaDetails(Long areaId) throws NotFoundException;

	List<Area> getAreaList(Boolean activeRecords, Long cityId) throws NotFoundException;

}
