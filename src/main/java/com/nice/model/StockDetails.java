package com.nice.model;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityResult;
import javax.persistence.FetchType;
import javax.persistence.FieldResult;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 28-Jan-2020
 */

@SqlResultSetMapping(name = "StockDetailsMapping", entities = @EntityResult(entityClass = StockDetails.class, fields = {
		@FieldResult(name = "id", column = "id"), @FieldResult(name = "active", column = "active"), @FieldResult(name = "createdAt", column = "created_at"),
		@FieldResult(name = "updatedAt", column = "updated_at"), @FieldResult(name = "createdBy", column = "created_by"),
		@FieldResult(name = "updatedBy", column = "updated_by"), @FieldResult(name = "productVariant", column = "product_variant_id"),
		@FieldResult(name = "available", column = "available"), @FieldResult(name = "reserved", column = "reserved"),
		@FieldResult(name = "replaced", column = "replaced"), @FieldResult(name = "delivered", column = "delivered"),
		@FieldResult(name = "expiryDate", column = "expiry_date"), @FieldResult(name = "lotDate", column = "lot_date"),
		@FieldResult(name = "lotNo", column = "lot_no"), @FieldResult(name = "vendorId", column = "vendor_id"), 
		@FieldResult(name = "returned", column = "returned"),
		@FieldResult(name = "expired", column = "expired"), }))

@Entity
@Table(name = "stock_details")
@Data
@EqualsAndHashCode(callSuper = false)
public class StockDetails extends CommonModel {

	/**
	 *
	 */
	private static final long serialVersionUID = -8321363361424422271L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id", nullable = false)
	private Long id;

	@Column(name = "lot_no", nullable = false)
	private Long lotNo;

	@JoinColumn(name = "product_variant_id", nullable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private ProductVariant productVariant;

	@Column(name = "vendor_id", nullable = false)
	private Long vendorId;

	@Column(name = "available")
	private Double available;

	@Column(name = "reserved")
	private Double reserved;

	@Column(name = "delivered")
	private Double delivered;

	@Column(name = "replaced")
	private Double replaced;

	@Column(name = "returned")
	private Double returned;

	@Column(name = "expired")
	private Double expired;

	@Column(name = "expiry_date")
	private Date expiryDate;

	@Column(name = "lot_date")
	private Date lotDate;

}
