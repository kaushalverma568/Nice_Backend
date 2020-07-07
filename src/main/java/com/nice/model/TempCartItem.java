package com.nice.model;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import lombok.Data;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Entity
@Table(name = "temp_cart_item")
@Data
public class TempCartItem implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -2503453724880223107L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "uuid", nullable = false)
	private String uuid;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE }, fetch = FetchType.LAZY)
	@JoinColumn(name = "product_variant_id", referencedColumnName = "id", nullable = false)
	private ProductVariant productVariant;

	@Column(name = "quantity", nullable = false)
	private Long quantity;

	@CreationTimestamp
	@Temporal(TemporalType.TIMESTAMP)
	@Column(name = "created_at", updatable = false, nullable = false)
	private Date createdAt;

	@Temporal(TemporalType.TIMESTAMP)
	@UpdateTimestamp
	@Column(name = "updated_at", nullable = false)
	private Date updatedAt;

	@Column(name = "active", nullable = false)
	private Boolean active;
}
