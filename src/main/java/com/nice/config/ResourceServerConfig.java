package com.nice.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableResourceServer;
import org.springframework.security.oauth2.config.annotation.web.configuration.ResourceServerConfigurerAdapter;
import org.springframework.security.oauth2.config.annotation.web.configurers.ResourceServerSecurityConfigurer;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Configuration
@EnableResourceServer
public class ResourceServerConfig extends ResourceServerConfigurerAdapter {

	private static final String RESOURCE_ID = "resource_id";

	private final Logger logger = LoggerFactory.getLogger(this.getClass());

	@Override
	public void configure(final ResourceServerSecurityConfigurer resources) {
		logger.info("Inside resource method ....");
		resources.resourceId(RESOURCE_ID).stateless(false);
	}

	@Override
	public void configure(final HttpSecurity http) throws Exception {
		http.antMatcher("/**").authorizeRequests().anyRequest().permitAll();
		http.cors();
		http.csrf().disable();

	}

}