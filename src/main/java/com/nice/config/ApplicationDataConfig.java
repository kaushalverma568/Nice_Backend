package com.nice.config;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:app-constants.properties")
@Data
public class ApplicationDataConfig {
    @Value("${vendor.featured.distance}")
    private Integer featuredVendorDistance;
}
