/**
 *
 */
package com.nice.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jun-2020
 */
@Configuration
@EnableJpaAuditing
public class JpaAuditingConfiguration {

	@Bean
	public UsernameAuditorAware auditorProvider() {
		return new UsernameAuditorAware();
	}
}
