package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.ModuleAndPermissionDTO;
import com.nice.dto.ModuleAndPermissionResponseDTO;
import com.nice.dto.PermissionDTO;
import com.nice.dto.RoleAndPermissionResponseDTO;
import com.nice.dto.RoleAndPermissionsDTO;
import com.nice.dto.RoleDTO;
import com.nice.dto.RoleResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PermissionMapper;
import com.nice.mapper.RoleMapper;
import com.nice.model.Modules;
import com.nice.model.Permission;
import com.nice.model.Role;
import com.nice.repository.PermissionRepository;
import com.nice.repository.RoleRepository;
import com.nice.service.ModulesService;
import com.nice.service.PermissionService;
import com.nice.service.RoleService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class RoleServiceImpl implements RoleService {

	private static final Logger LOGGER = LoggerFactory.getLogger(RoleServiceImpl.class);

	@Autowired
	private RoleRepository roleRepository;

	@Autowired
	private RoleMapper roleMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private PermissionService permissionService;

	@Autowired
	private PermissionMapper permissionMapper;

	@Autowired
	private PermissionRepository permissionRepository;

	@Autowired
	private ModulesService modulesService;

	@Override
	public void addRole(final RoleDTO roleDTO) {
		roleRepository.save(roleMapper.toEntity(roleDTO));
	}

	@Override
	public void updateRole(final RoleDTO roleDto) throws NotFoundException, ValidationException {
		if (roleDto.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("role.id.not.null", null));
		}
		getRole(roleDto.getId());
		roleRepository.save(roleMapper.toEntity(roleDto));
	}

	@Override
	public RoleResponseDTO getRole(final Long roleId) throws NotFoundException {
		return roleMapper.toDto(getRoleDetail(roleId));
	}

	@Override
	public Role getRoleDetail(final Long roleId) throws NotFoundException {
		return roleRepository.findById(roleId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("role.not.found", new Object[] { roleId })));
	}

	@Override
	public RoleAndPermissionResponseDTO getRoleDetailWithPermission(final Long roleId) throws NotFoundException {
		Role role = getRoleDetail(roleId);
		List<ModuleAndPermissionResponseDTO> moduleAndPermissionResponseDTOs = permissionMapper
				.toModuleAndPermissionResponseDTOs(permissionRepository.findAllByRoleAndActive(role, true));
		RoleAndPermissionResponseDTO roleAndPermissionsDTO = new RoleAndPermissionResponseDTO();
		roleAndPermissionsDTO.setModuleAndPermissionResponseDTOs(moduleAndPermissionResponseDTOs);
		roleAndPermissionsDTO.setActive(role.getActive());
		roleAndPermissionsDTO.setDescription(role.getDescription());
		roleAndPermissionsDTO.setId(roleId);
		roleAndPermissionsDTO.setName(role.getName());
		return roleAndPermissionsDTO;
	}

	@Override
	public Page<Role> getRoleList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			return roleRepository.findAllByActive(activeRecords, pageable);
		} else {
			return roleRepository.findAll(pageable);
		}
	}

	@Override
	public void changeStatus(final Long roleId, final Boolean isActive) throws ValidationException, NotFoundException {
		Role existingRole = getRoleDetail(roleId);
		LOGGER.info("Existing role details {} ", existingRole);
		if (isActive == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingRole.getActive().equals(isActive)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(isActive) ? "role.active" : "role.deactive", null));
		} else {
			if (Boolean.FALSE.equals(isActive)) {
				List<Permission> permissionList = permissionService.getPermissionList(existingRole, null, Boolean.TRUE);
				for (Permission Permission : permissionList) {
					permissionService.changeStatus(Permission.getId(), false);
				}
			}
			existingRole.setActive(isActive);
			roleRepository.save(existingRole);
		}
	}

	@Override
	public boolean isExists(final RoleDTO roleDto) {
		if (roleDto.getId() != null) {
			return roleRepository.findByNameIgnoreCaseAndIdNot(roleDto.getName(), roleDto.getId()).isPresent();
		} else {
			return roleRepository.findByNameIgnoreCase(roleDto.getName()).isPresent();
		}
	}

	@Override
	public void addUpdateRoleWithPermissions(final RoleAndPermissionsDTO roleAndPermissionsDTO) throws ValidationException, NotFoundException {
		RoleDTO roleDTO = new RoleDTO();
		BeanUtils.copyProperties(roleAndPermissionsDTO, roleDTO);
		Role role = roleRepository.save(roleMapper.toEntity(roleDTO));
		List<Long> modulesIds = new ArrayList<>();

		for (ModuleAndPermissionDTO moduleAndPermissionDTO : roleAndPermissionsDTO.getModuleAndPermissionDTOs()) {
			if (moduleAndPermissionDTO.getModulesId() == null) {
				throw new ValidationException(messageByLocaleService.getMessage("modules.id.not.null", null));
			}
			if (modulesIds.contains(moduleAndPermissionDTO.getModulesId())) {
				throw new ValidationException(messageByLocaleService.getMessage("modules.not.unique", null));
			}
			modulesIds.add(moduleAndPermissionDTO.getModulesId());
			Modules modules = modulesService.getModuleDetail(moduleAndPermissionDTO.getModulesId());
			/**
			 * add/update permission for this role and module
			 */
			PermissionDTO permissionDTO = new PermissionDTO();
			BeanUtils.copyProperties(moduleAndPermissionDTO, permissionDTO);
			Permission permission = permissionMapper.toEntity(permissionDTO);
			permission.setId(null);
			permission.setModules(modules);
			permission.setRole(role);
			permission.setActive(roleAndPermissionsDTO.getActive());
			Optional<Permission> optPermission = permissionRepository.findByRoleAndModules(role, modules);
			if (optPermission.isPresent()) {
				permission.setId(optPermission.get().getId());
			}
			permissionRepository.save(permission);
		}
	}

	@Override
	public void deleteRole(final Long roleId) throws NotFoundException, ValidationException {
		Role role = getRoleDetail(roleId);
		/**
		 * if any user contains this role then it can not be deleteable
		 */
		if (com.nice.constant.Role.getByValue(role.getName()) != null) {
			throw new ValidationException(messageByLocaleService.getMessage("role.not.deleteable", null));
		} else {
			List<Permission> permissions = permissionRepository.findAllByRole(role);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(permissions)) {
				LOGGER.info("delete permissions for role, permissions:{}", permissions);
				permissionRepository.deleteAll(permissions);
			}
		}
	}
}
