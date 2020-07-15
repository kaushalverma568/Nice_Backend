package com.nice.repository;

import java.math.BigInteger;
import java.sql.Timestamp;

import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.nice.constant.Constant;
import com.nice.exception.NotFoundException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Settings;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Repository
public class SettingsCustomRepositoryImpl implements SettingsCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(SettingsCustomRepositoryImpl.class);

	private static final String SETTINGS_FIELDS_FOR_UPDATE = "FIELD_NAME, FIELD_VALUE, ENCRYPTED, UPDATED_AT, UPDATED_BY";
	private static final String SETTINGS_FIELDS_FOR_INSERT = SETTINGS_FIELDS_FOR_UPDATE.concat(", CREATED_AT, CREATED_BY ");

	@Override
	public long addSettingsParameters(final Settings settings) {
		if (Boolean.TRUE.equals(settings.getEncrypted())) {
			LOGGER.info("Inside Settings DAO for adding settings, {}", settings);
			return ((BigInteger) entityManager
					.createNativeQuery("INSERT INTO SETTINGS (" + SETTINGS_FIELDS_FOR_INSERT + ") VALUES (?, PGP_SYM_ENCRYPT(?,'" + Constant.KEY_ENCRYPTION_SALT
							+ "'), ?, NOW(), ?, NOW(), ?) RETURNING ID")
					.setParameter(1, settings.getFieldName()).setParameter(2, settings.getFieldValue()).setParameter(3, settings.getEncrypted())
					.setParameter(5, settings.getCreatedBy()).setParameter(4, settings.getUpdatedBy()).getSingleResult()).longValue();
		} else {
			return ((BigInteger) entityManager
					.createNativeQuery("INSERT INTO SETTINGS (" + SETTINGS_FIELDS_FOR_INSERT + ") VALUES (?, ?, ?, NOW(), ?, NOW(), ?) RETURNING ID")
					.setParameter(1, settings.getFieldName()).setParameter(2, settings.getFieldValue()).setParameter(3, settings.getEncrypted())
					.setParameter(5, settings.getCreatedBy()).setParameter(4, settings.getUpdatedBy()).getSingleResult()).longValue();
		}

	}

	@Override
	public int updateSettingsParameters(final Settings settings) {
		/**
		 * If the value is to be stored in encrypted format
		 */
		if (Boolean.TRUE.equals(settings.getEncrypted())) {
			LOGGER.info("Inside Settings DAO for update encrypted settings, {}", settings);
			return entityManager
					.createNativeQuery("UPDATE SETTINGS SET FIELD_NAME =?, FIELD_VALUE = PGP_SYM_ENCRYPT(?,'" + Constant.KEY_ENCRYPTION_SALT
							+ "'), ENCRYPTED=?, UPDATED_AT=NOW(), UPDATED_BY=?  WHERE ID = ?")
					.setParameter(1, settings.getFieldName()).setParameter(2, settings.getFieldValue()).setParameter(3, settings.getEncrypted())
					.setParameter(4, settings.getUpdatedBy()).setParameter(5, settings.getId()).executeUpdate();
		}
		/**
		 * If the value is to be stored in non encrypted format
		 */
		else {
			LOGGER.info("Inside Settings DAO for update non encypted settings, {}", settings);
			return entityManager
					.createNativeQuery("UPDATE SETTINGS SET FIELD_NAME =?, FIELD_VALUE = ?, ENCRYPTED = ?, UPDATED_AT = NOW(), UPDATED_BY=? WHERE ID = ?")
					.setParameter(1, settings.getFieldName()).setParameter(2, settings.getFieldValue()).setParameter(3, settings.getEncrypted())
					.setParameter(4, settings.getUpdatedBy()).setParameter(5, settings.getId()).executeUpdate();
		}

	}

	@Override
	public Settings getDecryptedEntityValue(final String fieldName) throws NotFoundException {

		LOGGER.info("Inside Settings DAO for getting the encypted settings in decrypted format for field :, {}", fieldName);
		try {
			Object obj = entityManager
					.createNativeQuery("SELECT id, field_name, PGP_SYM_DECRYPT(cast(field_value as bytea),'" + Constant.KEY_ENCRYPTION_SALT
							+ "' ) as field_value, encrypted, active, updated_at, updated_by, created_at, created_by from settings WHERE field_name = ?")
					.setParameter(1, fieldName).getSingleResult();
			Object[] responseObj = (Object[]) obj;

			Settings settings = new Settings();
			settings.setId(Long.valueOf(responseObj[0].toString()));
			settings.setFieldName(String.valueOf(responseObj[1]));
			settings.setFieldValue(String.valueOf(responseObj[2]));
			settings.setEncrypted(Boolean.valueOf(responseObj[3].toString()));
			settings.setActive(Boolean.valueOf(responseObj[4].toString()));
			settings.setUpdatedAt((Timestamp) responseObj[5]);
			settings.setUpdatedBy(Long.valueOf(responseObj[6].toString()));
			settings.setCreatedAt((Timestamp) responseObj[7]);
			settings.setCreatedBy(Long.valueOf(responseObj[8].toString()));
			LOGGER.info("Before returning from Settings DAO for getting the encypted settings in decrypted format for field :, {}, the response Object : {}",
					fieldName, settings);
			return settings;
		} catch (NoResultException e) {
			throw new NotFoundException(messageByLocaleService.getMessage("settings.not.found.name", new Object[] { fieldName }));
		}
	}

}
