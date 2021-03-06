package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.dto.CategoryWiseProductCountDTO;
import com.nice.model.Product;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository(value = "productRepository")
public interface ProductRepository extends JpaRepository<Product, Long>, ProductCustomRepository {

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
	@Query("Select new com.nice.dto.CategoryWiseProductCountDTO(p.categoryId, count(*) ) from product p where p.vendorId = :vendorId and p.active= :active group by p.categoryId")
	List<CategoryWiseProductCountDTO> getCategoryWiseProductCountList(Long vendorId, Boolean active);

	/**
	 * @param vendorId
	 * @param cuisineId
	 * @return
	 */
	List<Product> findAllByVendorIdAndCategoryId(Long vendorId, Long categoryId);

	/**
	 * @param id
	 * @param id2
	 * @return
	 */
	List<Product> findAllByVendorIdAndCuisineId(Long vendorId, Long cuisineId);

	/**
	 * @param name
	 * @param cuisineId
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndCuisineIdAndVendorIdAndIdNot(String name, Long cuisineId, Long vendorId, Long id);

	/**
	 * @param name
	 * @param cuisineId
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndCuisineIdAndVendorIdAndIdNot(String name, Long cuisineId, Long vendorId, Long id);

	/**
	 * @param name
	 * @param brandId
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndCuisineIdAndVendorId(String name, Long brandId, Long vendorId);

	/**
	 * @param name
	 * @param brandId
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndCuisineIdAndVendorId(String name, Long brandId, Long vendorId);

	/**
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * @param name
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndVendorIdAndIdNot(String name, Long vendorId, Long id);

	/**
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param name
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndVendorId(String name, Long vendorId);

	/**
	 * @param name
	 * @param brandId
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndBrandIdAndVendorIdAndIdNot(String name, Long brandId, Long vendorId, Long id);

	/**
	 * @param name
	 * @param brandId
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameEnglishIgnoreCaseAndBrandIdAndVendorId(String name, Long brandId, Long vendorId);

	/**
	 * @param nameArabic
	 * @param brandId
	 * @param vendorId
	 * @param id
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndBrandIdAndVendorIdAndIdNot(String nameArabic, Long brandId, Long vendorId, Long id);

	/**
	 * @param nameArabic
	 * @param brandId
	 * @param vendorId
	 * @return
	 */
	Optional<Product> findByNameArabicIgnoreCaseAndBrandIdAndVendorId(String nameArabic, Long brandId, Long vendorId);

}
