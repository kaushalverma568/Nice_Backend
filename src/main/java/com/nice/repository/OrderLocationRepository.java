package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.OrderLocation;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 17-Jul-2020
 */
@Repository
public interface OrderLocationRepository extends JpaRepository<OrderLocation, Long> {

	void deleteAllByOrderId(Long orderId);

}
