CREATE TABLE IF NOT EXISTS OAUTH_CLIENT_DETAILS(
	CLIENT_ID CHARACTER VARYING(256) NOT NULL,
	RESOURCE_IDS CHARACTER VARYING(256),
	CLIENT_SECRET CHARACTER VARYING(256),
	SCOPE CHARACTER VARYING(256),
	AUTHORIZED_GRANT_TYPES CHARACTER VARYING(256),
	WEB_SERVER_REDIRECT_URI CHARACTER VARYING(256),
	AUTHORITIES CHARACTER VARYING(256),
	ACCESS_TOKEN_VALIDITY INTEGER,
	REFRESH_TOKEN_VALIDITY INTEGER,
	ADDITIONAL_INFORMATION CHARACTER VARYING(4096),
	AUTOAPPROVE CHARACTER VARYING(256),
	CONSTRAINT OAUTH_CLIENT_DETAILS_PKEY PRIMARY KEY (CLIENT_ID)
);

CREATE TABLE IF NOT EXISTS USER_LOGIN(
	ID BIGSERIAL NOT NULL,
	ENTITY_ID BIGINT ,
	ENTITY_TYPE CHARACTER VARYING(30),
	EMAIL CHARACTER VARYING(255),
	PHONE_NUMBER CHARACTER VARYING(20),
	OTP CHARACTER VARYING(255),
	PASSWORD CHARACTER VARYING(255),
	ROLE CHARACTER VARYING(55) NOT NULL,
	FACEBOOK_KEY CHARACTER VARYING(255),
	GOOGLE_KEY CHARACTER VARYING(255),
	ACTIVE BOOLEAN NOT NULL DEFAULT TRUE,
	CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
	CREATED_BY NUMERIC(10,0) NOT NULL,
	UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
	UPDATED_BY NUMERIC(10,0) NOT NULL,	
	CONSTRAINT USER_LOGIN_PKEY PRIMARY KEY (ID),
	CONSTRAINT USER_LOGIN_EMAIL_ENTITY_TYPE UNIQUE (ENTITY_TYPE, EMAIL)
);

INSERT INTO OAUTH_CLIENT_DETAILS(CLIENT_ID, RESOURCE_IDS, CLIENT_SECRET, SCOPE, AUTHORIZED_GRANT_TYPES, 
            WEB_SERVER_REDIRECT_URI, AUTHORITIES, ACCESS_TOKEN_VALIDITY,REFRESH_TOKEN_VALIDITY, ADDITIONAL_INFORMATION, AUTOAPPROVE)
    VALUES ('kody-client', 'resource_id', '$2a$12$ngzfuVDXy2XhNaGtqFlGF.91JsXehU.WChE18HY.yJBeCjtHHe8EW', 'trust,read,user_info,write',
    'refresh_token,password', '', 'ROLE_ADMIN', 30000, 40000, '{"web_server_redirect_uri":"","additional_information":""}', true);
   
INSERT INTO USER_LOGIN(
	ENTITY_ID, ENTITY_TYPE, EMAIL, PASSWORD, ROLE, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES (null,null,'kodytest.2020@gmail.com', '$2a$12$WMEtIan7v/2U8b2pnCRHb.zm/TH488DlUtZzYo0bSOyfBZT.yaZRa','SUPER_ADMIN',true, now(), 1,now(), 1);   

CREATE TABLE IF NOT EXISTS COUNTRY(
    ID BIGSERIAL NOT NULL,
	NAME CHARACTER VARYING(255) UNIQUE NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT COUNTRY_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS STATE(
    ID BIGSERIAL NOT NULL,
	COUNTRY_ID BIGINT NOT NULL,
	NAME CHARACTER VARYING(255) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT STATE_PKEY PRIMARY KEY (ID),
	CONSTRAINT STATE_COUNTRY FOREIGN KEY (COUNTRY_ID)
		REFERENCES COUNTRY (ID)
);

CREATE TABLE IF NOT EXISTS CITY(
    ID BIGSERIAL NOT NULL,
	STATE_ID BIGINT NOT NULL,
	NAME CHARACTER VARYING(255) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT CITY_PKEY PRIMARY KEY (ID),
	CONSTRAINT CITY_STATE FOREIGN KEY (STATE_ID)
		REFERENCES STATE (ID)
);

CREATE TABLE IF NOT EXISTS PINCODE
(
    ID BIGSERIAL NOT NULL,
    CODE_VALUE CHARACTER VARYING(255) NOT NULL,
    CITY_ID BIGINT NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT PINCODE_PKEY PRIMARY KEY (ID),
    CONSTRAINT PINCODE_CITY FOREIGN KEY (CITY_ID)
        REFERENCES CITY (ID)
);

CREATE TABLE IF NOT EXISTS PINCODE_HISTORY
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CODE_VALUE CHARACTER VARYING(255) NOT NULL,
    CREATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    PINCODE_ID BIGINT NOT NULL,
    CONSTRAINT PINCODE_HISTORY_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS COMPANY
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    COMPANY_ADDRESS CHARACTER VARYING(255) NOT NULL,
    COMPANY_EMAIL CHARACTER VARYING(255) NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
    CONTACT_NO CHARACTER VARYING(255) NOT NULL,
    CUSTOMER_CARE_EMAIL CHARACTER VARYING(255) NOT NULL,
    GSTIN CHARACTER VARYING(255) NOT NULL,
    COMPANY_IMAGE_NAME CHARACTER VARYING(255),
    COMPANY_IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    CONSTRAINT COMPANY_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS SETTINGS
(
    ID BIGSERIAL NOT NULL PRIMARY KEY,
	FIELD_NAME CHARACTER VARYING NOT NULL UNIQUE,
    FIELD_VALUE CHARACTER VARYING NOT NULL,
    ENCRYPTED BOOLEAN NOT NULL DEFAULT FALSE,
	ACTIVE BOOLEAN NOT NULL DEFAULT TRUE,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL
);

CREATE TABLE IF NOT EXISTS CUSTOMER
(
   ID BIGSERIAL NOT NULL,
   FIRST_NAME CHARACTER VARYING(20),
   LAST_NAME CHARACTER VARYING(20),
   EMAIL CHARACTER VARYING(255),
   PHONE_NUMBER CHARACTER VARYING(20),
   GENDER CHARACTER VARYING(20),
   BIRTH_DATE DATE, 
   REGISTERED_VIA CHARACTER VARYING(20) NOT NULL,
   EMAIL_VERIFIED BOOLEAN NOT NULL DEFAULT FALSE,
   MOBILE_VERIFIED BOOLEAN NOT NULL DEFAULT FALSE,
   STATUS VARCHAR(255),
   ACTIVE BOOLEAN NOT NULL,
   CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
   CREATED_BY BIGINT NOT NULL,
   UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
   UPDATED_BY BIGINT NOT NULL,
   CONSTRAINT CUSTOMER_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS CUSTOMER_ADDRESS
(
   ID BIGSERIAL NOT NULL,
   CUSTOMER_ID BIGINT NOT NULL,
   FIRST_NAME CHARACTER VARYING(255) NOT NULL,
   LAST_NAME CHARACTER VARYING(255) NOT NULL,
   PHONE_NUMBER CHARACTER VARYING(20),
   STREET_NO CHARACTER VARYING(255) NOT NULL,
   BUILDING_NAME CHARACTER VARYING(255) NOT NULL,
   LANDMARK CHARACTER VARYING(255) NOT NULL ,
   PINCODE_ID BIGINT NOT NULL ,
   COUNTRY_ID BIGINT NOT NULL,
   STATE_ID BIGINT NOT NULL,
   CITY_ID BIGINT NOT NULL,
   DEFAULT_ADDRESS BOOLEAN NOT NULL,
   ACTIVE BOOLEAN NOT NULL,
   CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
   CREATED_BY BIGINT NOT NULL,
   UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
   UPDATED_BY BIGINT NOT NULL,
   CONSTRAINT CUSTOMER_ADDRESS_PKEY PRIMARY KEY (ID),
   CONSTRAINT CUSTOMER_ADDRESS_CUSTOMER FOREIGN KEY (CUSTOMER_ID)
		REFERENCES CUSTOMER (ID),
   CONSTRAINT CUSTOMER_ADDRESS_COUNTRY FOREIGN KEY (COUNTRY_ID)
		REFERENCES COUNTRY (ID),
   CONSTRAINT CUSTOMER_ADDRESS_STATE FOREIGN KEY (STATE_ID)
		REFERENCES STATE (ID),
   CONSTRAINT CUSTOMER_ADDRESS_CITY FOREIGN KEY (CITY_ID)
		REFERENCES CITY (ID),
   CONSTRAINT CUSTOMER_ADDRESS_PINCODE FOREIGN KEY (PINCODE_ID)
		REFERENCES PINCODE (ID)
);

CREATE TABLE IF NOT EXISTS USER_OTP
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    OTP CHARACTER VARYING(255) NOT NULL,
    TYPE CHARACTER VARYING(255) NOT NULL,
    USER_LOGIN_ID BIGINT NOT NULL,
    CONSTRAINT USER_OTP_PKEY PRIMARY KEY (ID),
	CONSTRAINT USER_ID_TYPE_UNIQUE UNIQUE(USER_LOGIN_ID, TYPE),
    CONSTRAINT USER_LOGIN_ID_FK FOREIGN KEY (USER_LOGIN_ID)
        REFERENCES USER_LOGIN (ID)
);

CREATE EXTENSION IF NOT EXISTS pgcrypto;


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


INSERT INTO COUNTRY (NAME, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY, ACTIVE) VALUES ('INDIA', NOW(), 1, NOW(), 1, true);
INSERT INTO STATE (NAME, COUNTRY_ID, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY, ACTIVE) VALUES ('GUJARAT', 1,NOW(), 1, NOW(), 1, true);