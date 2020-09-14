INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SEND_SMS', false, false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SEND_EMAIL', false, false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('PAYMENT_GATEWAY_USER_NAME', PGP_SYM_ENCRYPT('default', 'kody_encryption_key'),true, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('PAYMENT_GATEWAY_SECRET', PGP_SYM_ENCRYPT('default', 'kody_encryption_key'), true, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SMS_API_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('LOGIN_FACEBOOK_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('LOGIN_GOOGLE_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('GOOGLE_MAP_KEY', 'DEFAULT', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('VENDOR_COMMISION_RATE', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_REPLACE_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_RETURN_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('ORDER_DELIVERY_CHARGE', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('DAY_MIN_ORDER_DELIVERED', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('INCENTIVE_AMOUNT_FOR_DAY', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('FACEBOOK_LINK', 'default', false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('TWITTER_LINK', 'default', false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('INSTAGRAM_LINK', 'default', false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('LINKEDIN_LINK', 'default', false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('YOUTUBE_LINK', 'default', false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('GOOGLE_LINK', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('ORDER_AMOUNT_FOR_FREE_DELIVERY', 0, false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('DELIVERY_CHARGE_DELIVERY_BOY_BELOW_MIN_ORDERS', 10, false, true, now(), 1, now(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('DELIVERY_CHARGE_DELIVERY_BOY_ABOVE_MIN_ORDERS', 15, false, true, now(), 1, now(), 1);