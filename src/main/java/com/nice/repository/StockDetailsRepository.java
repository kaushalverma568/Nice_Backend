package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.nice.model.ProductVariant;
import com.nice.model.StockDetails;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 28-Jan-2020
 */
public interface StockDetailsRepository extends JpaRepository<StockDetails, Long> {

	/**
	 * 
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<StockDetails> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * 
	 * @param productVariant2
	 * @return
	 */
	List<StockDetails> findByProductVariant(ProductVariant productVariant2);

	/**
	 * 
	 * @param lotNo
	 * @return
	 */
	StockDetails findByLotNo(Long lotNo);

	/**
	 * 
	 * @param productVariant
	 * @param lotNo
	 * @return
	 */
	Optional<StockDetails> findByProductVariantAndLotNo(ProductVariant productVariant, Long lotNo);

	/**
	 * find lot no from vendor and product variant
	 */
	@Query("Select lotNo from StockDetails sd where sd.productVariant.id = :productVariantId and sd.vendorId = :vendorId and sd.available > 0")
	List<Long> getLotNosWithQtyAvailable(Long vendorId, Long productVariantId);

	/**
	 * 
	 * @param runDate
	 * @param available
	 * @return
	 */
	List<StockDetails> findByExpiryDateLessThanAndAvailableGreaterThan(Date runDate, Double available);

	/**
	 * total available from product variant
	 * 
	 * @param productVariant
	 * @return
	 */
	@Query("select sum(sd.available) from StockDetails sd where productVariant=:productVariant")
	Long countAvailableQtyForProductVariantForVendor(ProductVariant productVariant);

	/**
	 * 
	 * @param productVariant
	 * @param vendorId
	 * @param lotNo
	 * @return
	 */
	Optional<StockDetails> findByProductVariantAndVendorIdAndLotNo(ProductVariant productVariant, Long vendorId, Long lotNo);

	/**
	 * 
	 * @param productvariant
	 * @param vendorId
	 * @return
	 */
	List<StockDetails> findByProductVariantAndVendorId(ProductVariant productvariant, Long vendorId);
}
