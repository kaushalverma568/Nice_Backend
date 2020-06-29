package com.nice.dto;

import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
@Data
public class DeviceDetailDTO {
	private Long id;
	@NotNull(message = "{device.id.not.null}")
	private String deviceId;
	@NotNull(message = "{user.type.not.null}")
	private String userType;
	@NotNull(message = "{user.id.not.null}")
	private Long userId;
	@NotNull(message = "{device.type.not.null}")
	private String deviceType;
	@NotNull(message = "{active.not.null}")
	private Boolean active;

}
