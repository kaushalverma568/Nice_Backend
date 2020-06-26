package com.nice;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;

import com.nice.util.FileStorageProperties;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 19-Jun-2020
 */
@EnableAutoConfiguration
@SpringBootApplication
@EnableConfigurationProperties({ FileStorageProperties.class })
public class NiceApplication {

	public static void main(final String[] args) {
		SpringApplication.run(NiceApplication.class, args);
	}

}
