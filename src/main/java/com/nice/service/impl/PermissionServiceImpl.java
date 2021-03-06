package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ModuleAndPermissionResponseDTO;
import com.nice.dto.PermissionDTO;
import com.nice.dto.PermissionResponseDTO;
import com.nice.dto.SideBarDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.PermissionMapper;
import com.nice.model.Modules;
import com.nice.model.Permission;
import com.nice.model.Role;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.PermissionRepository;
import com.nice.service.ModulesService;
import com.nice.service.PermissionService;
import com.nice.service.RoleService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class PermissionServiceImpl implements PermissionService {

	private static final Logger LOGGER = LoggerFactory.getLogger(PermissionServiceImpl.class);

	@Autowired
	private PermissionRepository permissionRepository;

	@Autowired
	private RoleService roleService;

	@Autowired
	private ModulesService modulesService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private PermissionMapper permissionMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Override
	public void addPermission(final PermissionDTO permissionDTO) throws NotFoundException {
		Permission permission = permissionMapper.toEntity(permissionDTO);
		permission.setModules(modulesService.getModuleDetail(permissionDTO.getModulesId()));
		permission.setRole(roleService.getRoleDetail(permissionDTO.getRoleId()));
		permissionRepository.save(permission);
	}

	@Override
	public void updatePermission(final PermissionDTO permissionDTO) throws NotFoundException, ValidationException {
		if (permissionDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("permission.id.not.null", null));
		}
		getPermissionDetail(permissionDTO.getId());
		Permission permission = permissionMapper.toEntity(permissionDTO);
		permission.setModules(modulesService.getModuleDetail(permissionDTO.getModulesId()));
		permission.setRole(roleService.getRoleDetail(permissionDTO.getRoleId()));
		permissionRepository.save(permission);
	}

	@Override
	public PermissionResponseDTO getPermission(final Long permissionId) throws NotFoundException {
		return permissionMapper.toResponseDTO(getPermissionDetail(permissionId));
	}

	@Override
	public Permission getPermissionDetail(final Long permissionId) throws NotFoundException {
		return permissionRepository.findById(permissionId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("permission.not.found", new Object[] { permissionId })));
	}

	@Override
	public Page<Permission> getPermissionList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Long roleId,
			final Long moduleId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			if (roleId != null) {
				Role role = roleService.getRoleDetail(roleId);
				if (moduleId != null) {
					Modules modules = modulesService.getModuleDetail(moduleId);
					return permissionRepository.findAllByActiveAndModulesAndRole(activeRecords, modules, role, pageable);
				} else {
					return permissionRepository.findAllByActiveAndRole(activeRecords, role, pageable);
				}
			} else {
				if (moduleId != null) {
					Modules module = modulesService.getModuleDetail(moduleId);
					return permissionRepository.findAllByActiveAndModules(activeRecords, module, pageable);
				} else {
					return permissionRepository.findAllByActive(activeRecords, pageable);
				}
			}
		} else {
			return getPermissonListByModulesAndRole(roleId, moduleId, pageable);
		}
	}

	/**
	 * @param  roleId
	 * @param  moduleId
	 * @param  pageable
	 * @return
	 * @throws NotFoundException
	 */
	private Page<Permission> getPermissonListByModulesAndRole(final Long roleId, final Long moduleId, final Pageable pageable) throws NotFoundException {
		if (moduleId != null) {
			Modules modules = modulesService.getModuleDetail(moduleId);
			if (roleId != null) {
				Role role = roleService.getRoleDetail(roleId);
				return permissionRepository.findAllByRoleAndModules(role, modules, pageable);
			} else {
				return permissionRepository.findAllByModules(modules, pageable);

			}
		} else {
			if (roleId != null) {
				Role role = roleService.getRoleDetail(roleId);
				return permissionRepository.findAllByRole(role, pageable);
			} else {
				return permissionRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long permissionId, final Boolean isActive) throws NotFoundException, ValidationException {
		Permission existing = getPermissionDetail(permissionId);
		LOGGER.info("Existing permission details {} ", existing);
		if (isActive == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existing.getActive().equals(isActive)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(isActive) ? "permission.active" : "permission.deactive", null));
		} else {
			existing.setActive(isActive);
			permissionRepository.save(existing);
		}
	}

	@Override
	public boolean isExists(final PermissionDTO permissionDTO, final Modules modules, final Role role) {

		if (permissionDTO.getId() != null) {
			/**
			 * At the time of update is role with same id for same module exist or not
			 * except it's own ID
			 */
			return permissionRepository.findByRoleAndModulesAndIdNot(role, modules, permissionDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is role with same id for same module exist or not
			 */
			return permissionRepository.findByRoleAndModules(role, modules).isPresent();
		}
	}

	@Override
	public Map<String, Boolean> getRoleAndModuleWisePermission(final Role role, final String moduleName) throws ValidationException, NotFoundException {
		Modules modules = modulesService.getModuleDetailByName(moduleName);
		Optional<Permission> permission = permissionRepository.findByRoleAndModules(role, modules);
		if (!permission.isPresent()) {
			throw new ValidationException(messageByLocaleService.getMessage("permission.not.found.role", new Object[] { role.getName(), moduleName }));
		}

		final Map<String, Boolean> permissionMap = new HashMap<>();
		permissionMap.put(Constant.CAN_ADD, permission.get().getCanAdd());
		permissionMap.put(Constant.CAN_EDIT, permission.get().getCanEdit());
		permissionMap.put(Constant.CAN_VIEW, permission.get().getCanView());
		permissionMap.put(Constant.CAN_DELETE, permission.get().getCanDelete());
		return permissionMap;
	}

	@Override
	public List<SideBarDTO> getSideBarSpectificPermissionListForUser() throws NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Vendor vendor = null;
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(userLogin.getEntityType()) && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendor = vendorService.getVendorDetail(userLogin.getEntityId());
		}
		Map<String, SideBarDTO> modulePermissionMap = new LinkedHashMap<>();
		List<SideBarDTO> sideBarDTOList = new ArrayList<>();

		/**
		 * get only those permissions for which particular role has access of VIEW Side
		 * bar
		 */
		List<Permission> permissionList = permissionRepository.findAllByRoleAndCanViewSidebarAndActiveOrderByModulesAsc(userLogin.getRole(), true, true);

		for (Permission permission : permissionList) {
			/**
			 * If there is no inventory manage for vendor then we will not display inventory
			 * module in side bar
			 */
			if (vendor != null) {
				if (vendor.getBusinessCategory().getManageInventory().booleanValue()) {
					if (permission.getModules().getName().equals("Product Attribute") || permission.getModules().getName().equals("Product Toppings")
							|| permission.getModules().getName().equals("Product Extras") || permission.getModules().getName().equals("Product Addons")) {
						continue;
					}
				} else {
					if (permission.getModules().getParentModuleName().equals("Inventory")) {
						continue;
					}
				}
			}
			if (modulePermissionMap.containsKey(permission.getModules().getParentModuleName())) {
				SideBarDTO sideBarDTO = modulePermissionMap.get(permission.getModules().getParentModuleName());
				List<ModuleAndPermissionResponseDTO> moduleWisePermissionList = sideBarDTO.getChildren();
				moduleWisePermissionList.add(permissionMapper.toModuleAndPermissionResponseDTO(permission));
				sideBarDTO.setChildren(moduleWisePermissionList);
				modulePermissionMap.put(permission.getModules().getParentModuleName(), sideBarDTO);
			} else {
				List<ModuleAndPermissionResponseDTO> moduleWisePermissionList = new ArrayList<>();
				SideBarDTO sideBarDTO = new SideBarDTO();
				if (!permission.getModules().getNoParentModule().booleanValue()) {
					ModuleAndPermissionResponseDTO moduleAndPermissionResponseDTO = permissionMapper.toModuleAndPermissionResponseDTO(permission);
					moduleWisePermissionList.add(moduleAndPermissionResponseDTO);
					sideBarDTO.setModulesId(1L);
					sideBarDTO.setCanView(true);
					sideBarDTO.setCanAdd(true);
					sideBarDTO.setCanEdit(true);
					sideBarDTO.setCanDelete(true);
					sideBarDTO.setCanViewSidebar(true);
				} else {
					sideBarDTO.setModulesId(permission.getModules().getId());
					sideBarDTO.setCanView(permission.getCanView());
					sideBarDTO.setCanAdd(permission.getCanAdd());
					sideBarDTO.setCanEdit(permission.getCanEdit());
					sideBarDTO.setCanDelete(permission.getCanDelete());
					sideBarDTO.setCanViewSidebar(permission.getCanViewSidebar());
				}
				sideBarDTO.setModulesName(permission.getModules().getParentModuleName());
				sideBarDTO.setChildren(moduleWisePermissionList);
				modulePermissionMap.put(permission.getModules().getParentModuleName(), sideBarDTO);

			}
		}
		for (Entry<String, SideBarDTO> parentModule : modulePermissionMap.entrySet()) {
			sideBarDTOList.add(parentModule.getValue());

		}

		return sideBarDTOList;
	}
}
