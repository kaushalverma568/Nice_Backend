package com.nice.repository;

import com.nice.exception.NotFoundException;
import com.nice.model.Settings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
public interface SettingsCustomRepository {

	long addSettingsParameters(final Settings settings);

	int updateSettingsParameters(final Settings settings);

	Settings getDecryptedEntityValue(final String entityName) throws NotFoundException;
}
