package com.nice.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Settings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Repository(value = "settingsRepository")
public interface SettingsRepository extends JpaRepository<Settings, Long>, SettingsCustomRepository {

	Optional<Settings> findByFieldNameIgnoreCase(String fieldName);

	Optional<Settings> findByFieldNameIgnoreCaseAndIdNot(String fieldName, Long settingsId);
}
