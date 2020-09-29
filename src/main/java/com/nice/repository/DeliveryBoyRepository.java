package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Jul-2020
 */
@Repository
public interface DeliveryBoyRepository extends JpaRepository<DeliveryBoy, Long>, DeliveryBoyCustomRepository {

	/**
	 * Get delivery boy by delivery boy email ignore case and id not equal if exist
	 *
	 * @param email
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
	 * find all by isLogin true,isBusy false,is available and not order id is null or not
	 *
	 * @param orderId
	 * @return
	 */
	// @Query("select d from DeliveryBoy d left join DeliveryBoySendNotificationHistory dn on d.id=dn.deliveryBoy.id join
	// DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id where dbcs.isLogin='true' and dbcs.isAvailable='true' and
	// dbcs.isBusy='false' and (dn.orderId IS NULL or dn.orderId != :orderId)")
	@Query("select d from DeliveryBoy d join DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id join UserLogin ul on d.id=ul.entityId join DeviceDetail dd on ul.id=dd.userLogin.id where dbcs.isLogin='true' and dbcs.isAvailable='true' and dbcs.isBusy='false' and d.id not in (select COALESCE(dbsh.deliveryBoy.id,0) from DeliveryBoySendNotificationHistory dbsh where dbsh.orderId=:orderId) and ul.entityType='DELIVERY_BOY'")
	List<DeliveryBoy> getAllNextAvailableDeliveryBoys(Long orderId);

	/**
	 * find all by isLogin true,is available and not order id is null or not
	 *
	 * @param orderId
	 * @return
	 */
	@Query("select d from DeliveryBoy d join DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id join UserLogin ul on d.id=ul.entityId join DeviceDetail dd on ul.id=dd.userLogin.id where dbcs.isLogin='true' and dbcs.isAvailable='true' and d.id not in (select COALESCE(dbsh.deliveryBoy.id,0) from DeliveryBoySendNotificationHistory dbsh where dbsh.orderId=:orderId) and ul.entityType='DELIVERY_BOY'")
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
	 * Get delivery boy list by active
	 *
	 * @param activeRecords
	 * @return
	 */
	List<DeliveryBoy> findAllByActive(Boolean activeRecords);

	/**
	 * Get delivery boy page By active and first name ignore case(arabic,english) or last name ignore case (arabic,english)
	 * contains keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param searchKeyword4
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByActiveAndFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
			Boolean activeRecords, String searchKeyword, String searchKeyword2, String searchKeyword3, String searchKeyword4, Pageable pageable);

	/**
	 * Get delivery boy page by first name ignore case(arabic,english) or last name ignore case (arabic,english) contains
	 * keyword
	 *
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param searchKeyword4
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
			String searchKeyword, String searchKeyword2, String searchKeyword3, String searchKeyword4, Pageable pageable);

	/**
	 * Get delivery boy list By active and first name ignore case(arabic,english) or last name ignore case (arabic,english)
	 * contains keyword
	 *
	 * @param activeRecords
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param searchKeyword4
	 * @param pageable
	 * @return
	 */
	List<DeliveryBoy> findAllByActiveAndFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
			Boolean activeRecords, String searchKeyword, String searchKeyword2, String searchKeyword3, String searchKeyword4);

	/**
	 * Get delivery boy list by first name ignore case(arabic,english) or last name ignore case (arabic,english) contains
	 * keyword
	 *
	 * @param searchKeyword
	 * @param searchKeyword2
	 * @param searchKeyword3
	 * @param searchKeyword4
	 * @param pageable
	 * @return
	 */
	List<DeliveryBoy> findAllByFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
			String searchKeyword, String searchKeyword2, String searchKeyword3, String searchKeyword4);

	/**
	 *
	 * @param isAvailable
	 * @param Active
	 * @return
	 */
	@Query("select count(*) from DeliveryBoy d inner join DeliveryBoyCurrentStatus dbcs on d.id=dbcs.deliveryBoy.id where  dbcs.isAvailable=:isAvailable and d.active =:active ")
	Long countByIsAvailableAndActive(boolean isAvailable, boolean active);

	/**
	 *
	 * @param status
	 * @param status2
	 * @return
	 */
	@Query("select count(*) from DeliveryBoy d where d.status =:status OR  d.status =:status2 ")
	Long getCountOfNewDeliveryBoys(String status, String status2);

}
