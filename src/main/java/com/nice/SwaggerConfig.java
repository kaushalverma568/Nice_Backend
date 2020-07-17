/**
 *
 */
package com.nice;

import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.bind.annotation.RequestMethod;

import com.google.common.base.Predicates;

import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.builders.ResponseMessageBuilder;
import springfox.documentation.schema.ModelRef;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 14-Jul-2020
 */
@Configuration
@EnableSwagger2
public class SwaggerConfig {

	/**
	 *
	 */
	private static final String NOT_FOUND_ERROR = "Not Found Error";
	/**
	 *
	 */
	private static final String VALIDATION_ERROR = "Validation Error";
	/**
	 *
	 */
	private static final String GENERIC_ERROR = "Generic Error";
	/**
	 *
	 */
	private static final String ERROR = "Error";

	@Bean
	public Docket postsApi() {
		return new Docket(DocumentationType.SWAGGER_2).select().apis(RequestHandlerSelectors.basePackage("com.nice.controller"))
				.paths(Predicates.not(PathSelectors.regex("/error.*"))).paths(Predicates.not(PathSelectors.regex("/oauth/authorize.*")))
				.paths(Predicates.not(PathSelectors.regex("/oauth/check_token.*"))).paths(Predicates.not(PathSelectors.regex("/actuator.*")))
				.paths(Predicates.not(PathSelectors.regex("/oauth/token_key.*"))).paths(Predicates.not(PathSelectors.regex("/oauth/confirm_access.*")))
				.paths(Predicates.not(PathSelectors.regex("/oauth/error.*"))).build().apiInfo(apiInfo()).useDefaultResponseMessages(false)
				.globalResponseMessage(RequestMethod.GET,
						List.of(new ResponseMessageBuilder().code(500).message(GENERIC_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(400).message(VALIDATION_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(404).message(NOT_FOUND_ERROR).responseModel(new ModelRef(ERROR)).build()))
				.globalResponseMessage(RequestMethod.POST,
						List.of(new ResponseMessageBuilder().code(500).message(GENERIC_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(400).message(VALIDATION_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(409).message(VALIDATION_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(404).message(NOT_FOUND_ERROR).responseModel(new ModelRef(ERROR)).build()))
				.globalResponseMessage(RequestMethod.PUT,
						List.of(new ResponseMessageBuilder().code(500).message(GENERIC_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(400).message(VALIDATION_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(409).message(VALIDATION_ERROR).responseModel(new ModelRef(ERROR)).build(),
								new ResponseMessageBuilder().code(404).message(NOT_FOUND_ERROR).responseModel(new ModelRef(ERROR)).build()));
	}

	private ApiInfo apiInfo() {
		return new ApiInfoBuilder().title("Nice Application").description("Nice Application API reference for developers").version("1.0").build();
	}
}
