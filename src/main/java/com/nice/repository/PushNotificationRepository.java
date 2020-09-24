package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.PushNotification;

/**
 * 
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Sep-2020
 */
@Repository
public interface PushNotificationRepository extends JpaRepository<PushNotification, Long> {
}
