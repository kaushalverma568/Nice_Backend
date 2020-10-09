package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Area;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Repository
public interface CustomerAddressRepository extends JpaRepository<CustomerAddress, Long>, CustomerAddressCustomRepository {

	/**
	 * Find active/deactive customer address for specific customer
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<CustomerAddress> findAllByCustomerAndActive(Customer customer, Boolean activeRecords, Pageable pageable);

	/**
	 * Find all customer address for specific customer
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<CustomerAddress> findAllByCustomer(Customer customer, Pageable pageable);

	/**
	 * To find by streetNo, Building name and landmark ignore case
	 *
	 * @param  streetNo
	 * @param  buildingName
	 * @param  area
	 * @param  id
	 * @return
	 */
	Optional<CustomerAddress> findByStreetNoAndBuildingNameAndAreaAndCustomer(String streetNo, String buildingName, Area area, Customer customer);

	/**
	 * To find by streetNo, Building name and landmark,area and customer and id not
	 *
	 * @param  streetNo
	 * @param  buildingName
	 * @param  area
	 * @param  customer
	 * @param  id
	 * @return
	 */
	Optional<CustomerAddress> findByStreetNoAndBuildingNameAndAreaAndCustomerAndIdNot(String streetNo, String buildingName, Area area, Customer customer,
			Long id);

	/**
	 * @param  customer
	 * @param  pageable
	 * @return
	 */
	List<CustomerAddress> findAllByCustomer(Customer customer);

	/**
	 * @param customer
	 */
	void deleteAllByCustomer(Customer customer);

	/**
	 * delete all by area
	 *
	 * @param area
	 */
	void deleteAllByArea(Area area);

}