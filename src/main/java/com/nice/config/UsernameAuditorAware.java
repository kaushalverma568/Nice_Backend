/**
 *
 */
package com.nice.config;

import java.util.Optional;

import org.springframework.data.domain.AuditorAware;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jun-2020
 */
public class UsernameAuditorAware implements AuditorAware<Long> {

	@Override
	public Optional<Long> getCurrentAuditor() {
		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

		if (authentication == null || !authentication.isAuthenticated()) {
			return Optional.ofNullable(2L);
		}

		if (authentication.getPrincipal().equals("anonymousUser")) {
			return Optional.ofNullable(2L);
		}

		return Optional.ofNullable(((UserAwareUserDetails) authentication.getPrincipal()).getUser().getId());
	}
}
