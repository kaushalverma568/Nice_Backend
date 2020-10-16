package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductVariant;
import com.nice.model.TempCartItem;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 03-Jul-2020
 */
@Repository
public interface TempCartItemRepository extends JpaRepository<TempCartItem, Long> {
	/**
	 * get CartItem by customerId , productId and productVariantId
	 *
	 * @param userLogin
	 * @param productVariant
	 * @return
	 */
	List<TempCartItem> findAllByUuidAndProductVariant(String uuid, ProductVariant productVariant);

	/**
	 * get number of cart item for user
	 *
	 * @param userLoginId
	 * @return
	 */
	Long countByUuid(String uuid);

	/**
	 * get all cart item by user login id
	 *
	 * @param userLoginId
	 * @param pageable
	 * @return
	 */
	List<TempCartItem> findAllByUuid(String uuid);

	/**
	 * @param productVariantId
	 * @return
	 */
	@Modifying
	@Query("delete from TempCartItem tci where tci.productVariant.id = :productVariantId")
	void deleteAllByProductVariantId(Long productVariantId);

	List<TempCartItem> findAllByProductVariantId(Long productVariantId);
}