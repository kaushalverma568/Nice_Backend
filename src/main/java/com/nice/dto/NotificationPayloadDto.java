/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Sep-2020
 */
@Data
public class NotificationPayloadDto implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 2076106220819459635L;
	private String module;
	private Long id;
	private String taskType = "";
}
