/**
 *
 */
package com.nice.dto;

import java.io.Serializable;

import lombok.Data;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : Apr 22, 2020
 * @description :
 */
@Data
public class SalesReportDto implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4574343629097946017L;
	private Double jan;
	private Double feb;
	private Double mar;
	private Double apr;
	private Double may;
	private Double jun;
	private Double jul;
	private Double aug;
	private Double sep;
	private Double oct;
	private Double nov;
	private Double dec;

}
