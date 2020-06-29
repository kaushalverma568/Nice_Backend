package com.nice.config;

import java.io.Serializable;
import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.PermissionEvaluator;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import com.nice.exception.ValidationException;
import com.nice.model.UserLogin;
import com.nice.service.PermissionService;
import com.nice.service.UserLoginService;

import lombok.Value;

@Component
class DomainAwarePermissionEvaluator implements PermissionEvaluator {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DomainAwarePermissionEvaluator.class);

	@Autowired
	private PermissionService permissionService;

	@Autowired
	private UserLoginService userLoginService;

	@Override
	public boolean hasPermission(final Authentication authentication, final Object targetDomainObject, final Object permission) {
		return hasRole(authentication, targetDomainObject, String.valueOf(permission));
	}

	private boolean hasRole(final Authentication authentication, final Object targetDomainObject, final String permissionName) {
		if (authentication.isAuthenticated()) {
			final Long userId = ((UserAwareUserDetails) authentication.getPrincipal()).getUser().getId();
			final String moduleName = (String) targetDomainObject;

			Boolean isAuthorized = false;

			try {
				final Optional<UserLogin> userLogin = userLoginService.getUserLogin(userId);

				if (userLogin.isPresent()) {
					final Map<String, Boolean> permission = permissionService.getRoleAndModuleWisePermission(userLogin.get().getRole(), moduleName);
					isAuthorized = permission.get(permissionName);
				}
			} catch (final ValidationException e) {
				LOGGER.info("Validation exception. :{}", e.getMessage());
			}

			return isAuthorized;

		} else {
			return false;
		}

	}

	@Override
	public boolean hasPermission(final Authentication authentication, final Serializable targetId, final String targetType, final Object permission) {
		return hasPermission(authentication, new DomainObjectReference(targetId, targetType), permission);
	}

	@Value
	static class DomainObjectReference {
		private final Long targetId;
		private final String targetType;

		public DomainObjectReference(final Serializable targetIdObj, final String targetTypeObj) {
			this.targetId = (Long) targetIdObj;
			this.targetType = targetTypeObj;
		}
	}

}
