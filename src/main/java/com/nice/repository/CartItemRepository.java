/**
 *
 */
package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.nice.model.CartItem;
import com.nice.model.Customer;
import com.nice.model.ProductVariant;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 06-Jul-2020
 */

public interface CartItemRepository extends JpaRepository<CartItem, Long> {

	/**
	 * get CartItem by customerId , productId and productVariantId
	 *
	 * @param userLogin
	 * @param productVariant
	 * @return
	 */
	List<CartItem> findAllByCustomerAndProductVariant(Customer customer, ProductVariant productVariant);

	/**
	 * get all cart item by user login id
	 *
	 * @param userLoginId
	 * @param pageable
	 * @return
	 */
	List<CartItem> findAllByCustomer(Customer customer);

	/**
	 * @param productVariantId
	 * @return
	 */
	@Modifying
	@Query("delete from CartItem ci where ci.customer.id = :customerId")
	void deleteAllByCustomerId(Long customerId);

	/**
	 * @param customer
	 * @return
	 */
	Long countByCustomer(Customer customer);
}
