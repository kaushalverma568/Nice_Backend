package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeliveryBoy;

@Repository
public interface DeliveryBoyRepository extends JpaRepository<DeliveryBoy, Long> {

	/**
	 * Get delivery boy by delivery boy email and id not equal if exist
	 *
	 * @param contactNo
	 * @param id
	 * @return
	 */
	Optional<DeliveryBoy> findByEmailAndIdNot(String email, Long id);

	/**
	 * Get delivery boy by delivery boy email if exist
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
	 * Get delivery boy page by active and isEmailVerified
	 *
	 * @param activeRecords
	 * @param isEmailVerified
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByActiveAndIsEmailVerified(Boolean activeRecords, Boolean isEmailVerified, Pageable pageable);

	/**
	 * Get delivery boy page by isEmailVerified
	 *
	 * @param isEmailVerified
	 * @param pageable
	 * @return
	 */
	Page<DeliveryBoy> findAllByIsEmailVerified(Boolean isEmailVerified, Pageable pageable);

	/**
	 * Get delivery boy list by isLoggedIn,isBusy and id not in
	 *
	 * @param isLoggedIn
	 * @param isBusy
	 * @param acceptDeliveryBoyIds
	 * @return
	 */
	List<DeliveryBoy> findAllByIsLoginAndIsBusyAndIdNotIn(Boolean isLoggedIn, Boolean isBusy, List<Long> acceptDeliveryBoyIds);

}
