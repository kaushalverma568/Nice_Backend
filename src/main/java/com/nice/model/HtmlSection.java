/**
 *
 */
package com.nice.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 07-Jan-2020
 */

@Data
@EqualsAndHashCode(callSuper = false)
@Entity
@Table(name = "html_section")
public class HtmlSection extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 538107667428292595L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "section_value", nullable = false)
	private String sectionValue;
	
	@Column(name = "section_type", nullable = false)
	private String sectionType;

}