package com.nice.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.ModulesDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ModulesMapper;
import com.nice.model.Modules;
import com.nice.repository.ModulesRepository;
import com.nice.service.ModulesService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 11, 2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ModulesServiceImpl implements ModulesService {

	@Autowired
	private ModulesRepository moduleRepository;

	@Autowired
	private ModulesMapper modulesMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public ModulesDTO getModule(final Long moduleId) throws NotFoundException, ValidationException {
		return modulesMapper.toDTO(getModuleDetail(moduleId));
	}

	@Override
	public Modules getModuleDetail(final Long moduleId) throws NotFoundException {
		return moduleRepository.findById(moduleId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("modules.not.found", new Object[] { moduleId })));
	}

	@Override
	public Modules getModuleDetailByName(final String name) throws NotFoundException {
		return moduleRepository.findByName(name)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("modules.not.found.name", new Object[] { name })));
	}

	@Override
	public Page<Modules> getModuleList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Boolean availableForNewRole) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (availableForNewRole != null) {
			if (activeRecords != null) {
				return moduleRepository.findAllByActiveAndAvailableForNewRole(activeRecords, availableForNewRole, pageable);
			} else {
				return moduleRepository.findAllByAvailableForNewRole(availableForNewRole, pageable);
			}
		} else {
			if (activeRecords != null) {
				return moduleRepository.findAllByActive(activeRecords, pageable);
			} else {
				return moduleRepository.findAll(pageable);
			}
		}
	}

}
