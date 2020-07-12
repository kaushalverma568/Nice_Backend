package com.nice.config;

import javax.sql.DataSource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.oauth2.common.exceptions.InvalidGrantException;
import org.springframework.security.oauth2.config.annotation.configurers.ClientDetailsServiceConfigurer;
import org.springframework.security.oauth2.config.annotation.web.configuration.AuthorizationServerConfigurerAdapter;
import org.springframework.security.oauth2.config.annotation.web.configuration.EnableAuthorizationServer;
import org.springframework.security.oauth2.config.annotation.web.configurers.AuthorizationServerEndpointsConfigurer;
import org.springframework.security.oauth2.provider.client.JdbcClientDetailsService;
import org.springframework.security.oauth2.provider.token.TokenStore;

import com.nice.locale.MessageByLocaleService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 19-Jun-2020
 */
@Configuration
@EnableAuthorizationServer
public class AuthorizationServerConfig extends AuthorizationServerConfigurerAdapter {

	private final Logger logger = LoggerFactory.getLogger(this.getClass());

	@Autowired
	private TokenStore tokenStore;

	@Autowired
	private DataSource datasource;

	@Autowired
	private AuthenticationManager authenticationManager;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Bean
	public JdbcClientDetailsService clientDetailsService() {
		return new JdbcClientDetailsService(datasource);
	}

	@Override
	public void configure(final ClientDetailsServiceConfigurer configurer) throws Exception {
		logger.info("Inside configuring client details for server......");
		configurer.jdbc(datasource);
	}

	@Override
	public void configure(final AuthorizationServerEndpointsConfigurer endpoints) {
		logger.info("Inside configuring endpoints for authorization.....");
		try {
			endpoints.tokenStore(tokenStore).authenticationManager(authenticationManager);

			endpoints.exceptionTranslator(exception -> {
				if (exception instanceof InternalAuthenticationServiceException) {
					InternalAuthenticationServiceException internalAuthenticationServiceException = (InternalAuthenticationServiceException) exception;
					return ResponseEntity.status(HttpStatus.OK).body(new CustomOauthException(internalAuthenticationServiceException.getMessage()));
				} else if (exception instanceof InvalidGrantException) {
					return ResponseEntity.status(HttpStatus.OK).body(new CustomOauthException(messageByLocaleService.getMessage("invaild.password", null)));
				}
				throw exception;
			});

		} catch (final Exception e) {
			logger.error("Error in configure oauth authorization ", e);
		}
	}
}