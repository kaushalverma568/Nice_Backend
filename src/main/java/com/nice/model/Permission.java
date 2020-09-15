
package com.nice.model;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Table(name = "permission")
@Entity
@Data
@EqualsAndHashCode(callSuper = false)
public class Permission extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -2602264881034245285L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	@JoinColumn(name = "role_id")
	private Role role;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE })
	@JoinColumn(name = "modules_id")
	private Modules modules;

	@Column(name = "can_add", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
	private Boolean canAdd;

	@Column(name = "can_edit", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
	private Boolean canEdit;

	@Column(name = "can_view", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
	private Boolean canView;

	@Column(name = "can_delete", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
	private Boolean canDelete;

	@Column(name = "side_bar", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
	private Boolean sideBar;
}
