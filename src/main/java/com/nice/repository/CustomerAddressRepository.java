package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Customer;
import com.nice.model.CustomerAddress;
import com.nice.model.Pincode;

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
	 * To find by streetNo, Building name and landmark and pincode ignore case
	 *
	 * @param  streetNo
	 * @param  buildingName
	 * @param  area
	 * @param  id
	 * @return
	 */
	Optional<CustomerAddress> findByStreetNoAndBuildingNameAndAreaAndPincodeAndCustomer(String streetNo, String buildingName, String area, Pincode pincode,
			Customer customer);

	/**
	 * To find by streetNo, Building name and landmark and pincode and customer and id not
	 *
	 * @param  streetNo
	 * @param  buildingName
	 * @param  area
	 * @param  pincode
	 * @param  customer
	 * @param  id
	 * @return
	 */
	Optional<CustomerAddress> findByStreetNoAndBuildingNameAndAreaAndPincodeAndCustomerAndIdNot(String streetNo, String buildingName, String area,
			Pincode pincode, Customer customer, Long id);

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
	 * @param pincode
	 */
	void deleteAllByPincode(Pincode pincode);

}