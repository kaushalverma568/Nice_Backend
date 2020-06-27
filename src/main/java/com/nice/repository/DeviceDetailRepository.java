package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.DeviceDetail;
import com.nice.model.UserLogin;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 28-Apr-2020
 */
@Repository
public interface DeviceDetailRepository extends JpaRepository<DeviceDetail, Long> {

	Optional<DeviceDetail> findByUserLogin(UserLogin userLogin);

	Optional<DeviceDetail> findByDeviceIdAndUserTypeAndIdNot(String deviceId, String userType, Long id);

	Optional<DeviceDetail> findByDeviceIdAndUserType(String deviceId, String userType);

}
