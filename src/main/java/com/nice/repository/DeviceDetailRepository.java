package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.DeviceDetail;
import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository
public interface DeviceDetailRepository extends JpaRepository<DeviceDetail, Long> {

	Optional<DeviceDetail> findByUserLogin(UserLogin userLogin);

	Optional<DeviceDetail> findByDeviceIdAndUserTypeAndIdNot(String deviceId, String userType, Long id);

	Optional<DeviceDetail> findByDeviceIdAndUserType(String deviceId, String userType);

	/**
	 * @param deviceId
	 */
	@Modifying
	@Query(value = "delete from DeviceDetail where deviceId = :deviceId")
	void deleteByDeviceId(String deviceId);
}
