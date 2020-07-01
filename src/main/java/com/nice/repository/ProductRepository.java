package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.dto.CuisineWiseProductCountDTO;
import com.nice.model.Product;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository(value = "productRepository")
public interface ProductRepository extends JpaRepository<Product, Long>, ProductCustomRepository {

	/**
	 * @param name
	 * @param brandId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameIgnoreCaseAndBrandIdAndVendorIdAndIdNot(String name, Long brandId, Long vendorId, Long id);

	/**
	 * @param name
	 * @param brandId
	 * @return
	 */
	Optional<Product> findByNameIgnoreCaseAndBrandIdAndVendorId(String name, Long brandId, Long vendorId);

	/**
	 * @param pageable
	 * @param vendorId
	 * @return
	 */
	Page<Product> findAllByVendorId(Pageable pageable, Long vendorId);

	/**
	 *
	 * @param vendorId
	 * @param active
	 * @return
	 */
	@Query("Select new com.nice.dto.CuisineWiseProductCountDTO(p.cuisineId, count(*) ) from product p where p.vendorId = :vendorId and p.active= :active group by p.cuisineId")
	List<CuisineWiseProductCountDTO> getCuisineWiseProductCountList(Long vendorId, Boolean active);

	/**
	 * @param vendorId
	 * @param cuisineId
	 * @return
	 */
	List<Product> findAllByVendorIdAndCuisineId(Long vendorId, Long cuisineId);

}
