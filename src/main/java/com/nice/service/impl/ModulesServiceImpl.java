package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import com.nice.model.Permission;
import com.nice.repository.ModulesRepository;
import com.nice.service.ModulesService;
import com.nice.service.PermissionService;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ModulesServiceImpl implements ModulesService {

	private static final Logger LOGGER = LoggerFactory.getLogger(ModulesServiceImpl.class);

	@Autowired
	private ModulesRepository moduleRepository;

	@Autowired
	private ModulesMapper modulesMapper;

	@Autowired
	private PermissionService permissionService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addModule(final ModulesDTO modulesDTO) {
		moduleRepository.save(modulesMapper.toEntity(modulesDTO));
	}

	@Override
	public void updateModule(final ModulesDTO modulesDTO) throws NotFoundException, ValidationException {
		if (modulesDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("modules.id.not.null", null));
		}
		getModule(modulesDTO.getId());
		moduleRepository.save(modulesMapper.toEntity(modulesDTO));
	}

	@Override
	public ModulesDTO getModule(final Long moduleId) throws NotFoundException {
		return modulesMapper.toDTO(getModuleDetail(moduleId));
	}

	@Override
	public Modules getModuleDetail(final Long moduleId) throws NotFoundException {
		return moduleRepository.findById(moduleId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("modules.not.found", new Object[] { moduleId })));
	}

	@Override
	public Page<Modules> getModuleList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			return moduleRepository.findAllByActive(activeRecords, pageable);
		} else {
			return moduleRepository.findAll(pageable);
		}
	}

	@Override
	public void changeStatus(final Long moduleId, final Boolean isActive) throws ValidationException, NotFoundException {
		Modules existingModules = getModuleDetail(moduleId);
		LOGGER.info("Existing module details {} ", existingModules);
		if (isActive == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingModules.getActive().equals(isActive)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(isActive) ? "modules.active" : "modules.deactive", null));
		} else {
			if (Boolean.FALSE.equals(isActive)) {
				List<Permission> permissionList = permissionService.getPermissionList(null, existingModules, Boolean.TRUE);
				for (Permission Permission : permissionList) {
					permissionService.changeStatus(Permission.getId(), false);
				}
			}
			existingModules.setActive(isActive);
			moduleRepository.save(existingModules);
		}
	}

	@Override
	public boolean isExists(final ModulesDTO modulesDTO) {
		if (modulesDTO.getId() != null) {
			return moduleRepository.findByNameIgnoreCaseAndIdNot(modulesDTO.getName(), modulesDTO.getId()).isPresent();
		} else {
			return moduleRepository.findByNameIgnoreCase(modulesDTO.getName()).isPresent();
		}
	}
}
