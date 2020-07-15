package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Product;
import com.nice.model.ProductExtras;
import com.nice.model.ProductExtrasMaster;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Repository
public interface ProductExtrasRepository extends JpaRepository<ProductExtras, Long> {

	/**
	 *
	 * @param product
	 * @param active
	 * @return
	 */
	List<ProductExtras> findAllByProductAndActive(Product product, Boolean active);

	/**
	 *
	 * @param product
	 * @return
	 */
	List<ProductExtras> findAllByProduct(Product product);


	/**
	 * @param vendorId
	 * @param pageable
	 * @return
	 */
	Page<ProductExtras> findAllByVendorId(Long vendorId, Pageable pageable);

	/**
	 * 
	 * @param product
	 * @param extrasMaster
	 * @param id
	 * @return
	 */
	Optional<ProductExtras> findByProductAndProductExtrasMasterAndIdNot(Product product,
			ProductExtrasMaster extrasMaster, Long id);

	/**
	 * 
	 * @param product
	 * @param extrasMaster
	 * @return
	 */
	Optional<ProductExtras> findByProductAndProductExtrasMaster(Product product, ProductExtrasMaster extrasMaster);

	/**
	 * 
	 * @param productExtrasMaster
	 * @param activeRecords
	 * @return
	 */
	List<ProductExtras> findAllByProductExtrasMasterAndActive(ProductExtrasMaster productExtrasMaster,
			Boolean activeRecords);

	/**
	 * 
	 * @param productExtrasMaster
	 * @return
	 */
	List<ProductExtras> findAllByProductExtrasMaster(ProductExtrasMaster productExtrasMaster);

}
