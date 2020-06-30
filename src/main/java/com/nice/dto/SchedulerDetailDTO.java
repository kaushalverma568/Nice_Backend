
package com.nice.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 25-Dec-2019
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class SchedulerDetailDTO implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6817634119754001981L;

	private Long id;

	private String name;

	private Date updatedAt;
	/**
	 * if scheduler is runned for today then set to true
	 */
	private Boolean isRunned;
}
