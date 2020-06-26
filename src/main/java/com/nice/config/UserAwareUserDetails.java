/**
 *
 */
package com.nice.config;

import java.util.Collection;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.User;

import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jun-2020
 */
public class UserAwareUserDetails extends User {

	/**
	 *
	 */
	private static final long serialVersionUID = -5934947697196738307L;
	private UserLogin user;

	/**
	 * @param username
	 * @param password
	 * @param authorities
	 * @param user
	 */
	public UserAwareUserDetails(final String username, final String password, final Collection<? extends GrantedAuthority> authorities, final UserLogin user) {
		super(username, password, authorities);
		this.user = user;
	}

	/**
	 * @param username
	 * @param password
	 * @param enabled
	 * @param accountNonExpired
	 * @param credentialsNonExpired
	 * @param accountNonLocked
	 * @param authorities
	 */
	public UserAwareUserDetails(final String username, final String password, final boolean enabled, final boolean accountNonExpired,
			final boolean credentialsNonExpired, final boolean accountNonLocked, final Collection<? extends GrantedAuthority> authorities) {
		super(username, password, enabled, accountNonExpired, credentialsNonExpired, accountNonLocked, authorities);
	}

	/**
	 * @param username
	 * @param password
	 * @param authorities
	 */
	public UserAwareUserDetails(final String username, final String password, final Collection<? extends GrantedAuthority> authorities) {
		super(username, password, authorities);
	}

	public UserLogin getUser() {
		return user;
	}

	public void setUser(final UserLogin user) {
		this.user = user;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((user == null) ? 0 : user.hashCode());
		return result;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (!super.equals(obj)) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		UserAwareUserDetails other = (UserAwareUserDetails) obj;
		if (user == null) {
			if (other.user != null) {
				return false;
			}
		} else if (!user.equals(other.user)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("UserAwareUserDetails [user=").append(user).append("]");
		return builder.toString();
	}

}
