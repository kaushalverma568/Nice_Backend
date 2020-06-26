package com.nice.model;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@MappedSuperclass
@Data
@EntityListeners(AuditingEntityListener.class)
public class CommonModel implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = 7869831537646377462L;
	@Column(name = "active", nullable = false)
	private Boolean active;

	@CreationTimestamp
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "created_at", updatable = false, nullable = false)
	private Date createdAt;

	@Temporal(TemporalType.TIMESTAMP)
	@UpdateTimestamp
	@Column(name = "updated_at", nullable = false)
	private Date updatedAt;

	@CreatedBy
	@Column(name = "created_by", updatable = false, nullable = false)
	private Long createdBy;

	@LastModifiedBy
	@Column(name = "updated_by", nullable = false)
	private Long updatedBy;

}
