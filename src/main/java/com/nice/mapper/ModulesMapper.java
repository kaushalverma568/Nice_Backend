/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ModulesDTO;
import com.nice.model.Modules;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Component
public class ModulesMapper {

	public ModulesDTO toDTO(final Modules modules) {
		ModulesDTO modulesDTO = new ModulesDTO();
		BeanUtils.copyProperties(modules, modulesDTO);
		return modulesDTO;
	}

	public Modules toEntity(final ModulesDTO modulesDTO) {
		Modules modules = new Modules();
		BeanUtils.copyProperties(modulesDTO, modules);
		return modules;
	}

	public List<ModulesDTO> toDTOs(final List<Modules> modulesList) {
		List<ModulesDTO> modulesDTOList = new ArrayList<>();
		for (Modules modules : modulesList) {
			modulesDTOList.add(toDTO(modules));
		}
		return modulesDTOList;
	}
}
