/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.UOM;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Jun-2020
 */
@Repository("productVariantRepository")
public interface ProductVariantRepository extends JpaRepository<ProductVariant, Long> {

	List<ProductVariant> findAllByProduct(Product product);

	List<ProductVariant> findAllByProductAndActive(Product product, Boolean active);

	Optional<ProductVariant> findByProductAndUom(Product product, UOM uom);

	Optional<ProductVariant> findByProductAndUomAndIdNot(Product product, UOM uom, Long productvariantId);

	/**
	 * @param sku
	 */
	Optional<ProductVariant> findBySkuIgnoreCase(String sku);

	/**
	 * @param sku
	 * @param id
	 * @return
	 */
	Optional<ProductVariant> findBySkuIgnoreCaseAndIdNot(String sku, Long id);

	/**
	 * @param sku
	 * @param vendorId
	 * @return
	 */
	Optional<ProductVariant> findBySkuIgnoreCaseAndVendorId(String sku, Long vendorId);

}
