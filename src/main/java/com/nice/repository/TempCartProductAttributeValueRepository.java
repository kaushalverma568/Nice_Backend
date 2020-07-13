/**
 *
 */
package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.ProductAttributeValue;
import com.nice.model.TempCartItem;
import com.nice.model.TempCartProductAttributeValue;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Jul-2020
 */
@Repository
public interface TempCartProductAttributeValueRepository extends JpaRepository<TempCartProductAttributeValue, Long> {

	/**
	 * @param cartItem
	 */
	void deleteAllByTempCartItem(TempCartItem cartItem);

	/**
	 * @param tempCartItem
	 * @param productAttributeValue
	 * @return
	 */
	Optional<TempCartProductAttributeValue> findAllByTempCartItemAndProductAttributeValue(TempCartItem tempCartItem,
			ProductAttributeValue productAttributeValue);

	/**
	 * @param tempCartItem
	 * @return
	 */
	List<TempCartProductAttributeValue> findAllByTempCartItem(TempCartItem tempCartItem);

	/**
	 * @param tempCartItem
	 * @param productAttribute
	 * @return
	 */
	@Query(value = "select count(*) from TempCartProductAttributeValue tcpav where tcpav.tempCartItem.id = :tempCartItemId and tcpav.productAttributeValue.productAttribute.id =:productAttributeId")
	Long getCountByTempCartItemAndProductAttribute(Long tempCartItemId, Long productAttributeId);

}
