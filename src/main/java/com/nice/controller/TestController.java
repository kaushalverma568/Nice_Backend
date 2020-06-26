/**
 *
 */
package com.nice.controller;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jun-2020
 */
@RestController
@RequestMapping("/")
public class TestController {

	@Value("${application.name}")
	private String applicationName;

	@GetMapping
	public String test() {
		return "Welcome to " + applicationName;
	}
}
