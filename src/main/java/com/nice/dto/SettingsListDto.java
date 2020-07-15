
package com.nice.dto;

import java.io.Serializable;
import java.util.List;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Data
public class SettingsListDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2133542493619752171L;
	private List<SettingsDto> settingDtoList;

}
