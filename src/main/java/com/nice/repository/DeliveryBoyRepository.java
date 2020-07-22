package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;

@Repository
public interface DeliveryBoyRepository extends JpaRepository<DeliveryBoy, Long> {

	/**
	 * Get delivery boy by delivery boy email ignore case and id not equal if exist
	 *
	 * @param contactNo
	 * @param id
	 * @return
	 */
	Optional<DeliveryBoy> findByEmailAndIdNot(String email, Long id);

	/**
	 * Get delivery boy by delivery boy email ignore case if exist
	 *
	 * @param email
	 * @return
	 */
	Optional<DeliveryBoy> findByEmail(String email);

	/**
	 * Get delivery boy page by active
	 *
	 * @param activeRecords
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * find all by isLogin true,isBusy false,is available and not order id is null
	 * or not
	 *
	 * @param orderId
	 * @return
	 */
	@Query("select d from DeliveryBoy d left join DeliveryBoySendNotificationHistory dn on d.id=dn.deliveryBoy.id join DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id where dbcs.isLogin='true' and dbcs.isAvailable='true' and dbcs.isBusy='false' and (dn.orderId IS NULL or dn.orderId != :orderId)")
	List<DeliveryBoy> getAllNextAvailableDeliveryBoys(Long orderId);

	/**
	 * find all by isLogin true,is available and not order id is null or not
	 *
	 * @param orderId
	 * @return
	 */
	@Query("select d from DeliveryBoy d left join DeliveryBoySendNotificationHistory dn on d.id=dn.deliveryBoy.id join DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id where dbcs.isLogin='true' and dbcs.isAvailable='true' and (dn.orderId IS NULL or dn.orderId != :orderId)")
	List<DeliveryBoy> getAllNextAvailableDeliveryBoysOnBusyTime(Long orderId);

	/**
	 * find by phone number ignore case and id not
	 *
	 * @param phoneNumber
	 * @param id
	 * @return
	 */
	Optional<DeliveryBoy> findByPhoneNumberIgnoreCaseAndIdNot(String phoneNumber, Long id);

	/**
	 * find by phone number
	 *
	 * @param phoneNumber
	 * @return
	 */
	Optional<DeliveryBoy> findByPhoneNumberIgnoreCase(String phoneNumber);

	/**
	 * Get delivery boy page by first name or last name
	 *
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(String searchKeyword, String searchKeyword2, Pageable pageable);

	/**
	 * Get delivery boy page by active and first name or last name
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByActiveAndFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(Boolean activeRecords, String searchKeyword,
			String searchKeyword2, Pageable pageable);

	/**
	 *
	 * @param activeRecords
	 * @return
	 */
	List<DeliveryBoy> findAllByActive(Boolean activeRecords);

}
