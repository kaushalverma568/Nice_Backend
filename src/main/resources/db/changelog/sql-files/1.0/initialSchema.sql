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

CREATE EXTENSION IF NOT EXISTS PGCRYPTO;

CREATE TABLE IF NOT EXISTS HTML_SECTION
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    SECTION_VALUE TEXT NOT NULL,
	SECTION_TYPE  CHARACTER VARYING(255),
    CONSTRAINT SECTION_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS SLIDER_IMAGE
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    IMAGE_NAME CHARACTER VARYING(255) NOT NULL,
    ORIGIONAL_IMAGE_NAME CHARACTER VARYING(255) NOT NULL,
  	IMAGE_TYPE CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT SLIDER_BANNER_PKEY PRIMARY KEY (ID)
);     

  CREATE TABLE IF NOT EXISTS BRAND
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT BRAND_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS ROLE
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    DESCRIPTION CHARACTER VARYING(255),
    NAME CHARACTER VARYING(255),
    CONSTRAINT ROLE_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS MODULES
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NAME CHARACTER VARYING(255),
    CONSTRAINT MODULES_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS PERMISSION
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CAN_ADD BOOLEAN,
    CAN_DELETE BOOLEAN,
    CAN_EDIT BOOLEAN,
    CAN_EXPORT BOOLEAN,
    CAN_IMPORT BOOLEAN,
    CAN_VIEW BOOLEAN,
    CAN_VIEW_LIST BOOLEAN,
    MODULES_ID BIGINT,
    ROLE_ID BIGINT,
    CONSTRAINT PERMISSION_PKEY PRIMARY KEY (ID),
    CONSTRAINT PERMISSION_MODULES FOREIGN KEY (MODULES_ID)
        REFERENCES MODULES (ID),
    CONSTRAINT PERMISSION_ROLE FOREIGN KEY (ROLE_ID)
        REFERENCES ROLE (ID)
);


CREATE TABLE IF NOT EXISTS DEVICE_DETAIL
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    DEVICE_ID CHARACTER VARYING(255) NOT NULL,
    USER_TYPE CHARACTER VARYING(255) NOT NULL,
    DEVICE_TYPE CHARACTER VARYING(255) NOT NULL,
    USER_ID BIGINT NOT NULL,
    CONSTRAINT DEVICE_DETAIL_PKEY PRIMARY KEY (ID),
    CONSTRAINT DEVICE_DETAIL_USER_LOGIN FOREIGN KEY (USER_ID)
        REFERENCES USER_LOGIN (ID),
    UNIQUE (USER_TYPE, DEVICE_ID)
);
	
CREATE TABLE IF NOT EXISTS USERS
(
    ID BIGSERIAL NOT NULL,
    FIRST_NAME CHARACTER VARYING(255) NOT NULL,
    LAST_NAME CHARACTER VARYING(255) NOT NULL,
    EMAIL CHARACTER VARYING(255) NOT NULL,
    ROLE CHARACTER VARYING(255) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT USERS_PKEY PRIMARY KEY (ID),
    CONSTRAINT USERS_UNIQUE_EMAIL UNIQUE (EMAIL)
);

CREATE TABLE IF NOT EXISTS UOM
(
    ID BIGSERIAL NOT NULL,
    MEASUREMENT CHARACTER VARYING(128) NOT NULL,
    UOM_LABEL CHARACTER VARYING(255) NOT NULL,
    QUANTITY NUMERIC(10,2) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL, 
    VENDOR_ID BIGINT,
    CONSTRAINT UOM_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS BUSINESS_CATEGORY
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
	IMAGE_NAME CHARACTER VARYING(255),
	ORIGINAL_IMAGE_NAME CHARACTER VARYING(255),
	MANAGE_INVENTORY BOOLEAN NOT NULL,
    CONSTRAINT VENDOR_CATEGORY_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS SUBSCRIPTION_PLAN
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
	AMOUNT   NUMERIC(10,2) NOT NULL,
	DAYS INTEGER NOT NULL,
	IMAGE_NAME CHARACTER VARYING(255),
	ORIGINAL_IMAGE_NAME CHARACTER VARYING(255),
	DESCRIPTION CHARACTER VARYING(255),
    CONSTRAINT SUBSCRIPTION_PLAN_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS CUISINE
(
    ID BIGSERIAL NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    IMAGE_NAME CHARACTER VARYING(255),
	IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
   	CONSTRAINT CUISINE_PKEY PRIMARY KEY (ID),
    CONSTRAINT CUISINE_UKEY UNIQUE (NAME)
);

CREATE TABLE IF NOT EXISTS SCHEDULER_DETAILS
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
    NAME CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT SCHEDULER_DETAILS_PKEY PRIMARY KEY (ID)
);
	
	
CREATE TABLE IF NOT EXISTS SETTING_HISTORY
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CURRENT_FIELD_VALUE CHARACTER VARYING(255) NOT NULL,
    FIELD_NAME CHARACTER VARYING(255)  NOT NULL,
    PREVIOUS_FIELD_VALUE CHARACTER VARYING(255)  NOT NULL,
    CONSTRAINT SETTING_HISTORY_PKEY PRIMARY KEY (ID)
);
	


CREATE TABLE IF NOT EXISTS TICKET
(
    ID BIGSERIAL NOT NULL,
 	CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  	CREATED_BY NUMERIC(10,0) NOT NULL,
  	UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  	UPDATED_BY NUMERIC(10,0) NOT NULL,
  	ACTIVE BOOLEAN NOT NULL,
    COMMENT CHARACTER VARYING(255),
    DESCRIPTION CHARACTER VARYING(255),
    EMAIL CHARACTER VARYING(255) NOT NULL,
    TICKET_REASON CHARACTER VARYING(255) NOT NULL,
    TICKET_STATUS CHARACTER VARYING(255) NOT NULL,
    USER_TYPE CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT TICKET_PKEY PRIMARY KEY (ID)
);

CREATE TABLE IF NOT EXISTS TICKET_REASON
(
    ID BIGSERIAL NOT NULL,
 	CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  	CREATED_BY NUMERIC(10,0) NOT NULL,
  	UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  	UPDATED_BY NUMERIC(10,0) NOT NULL,
  	ACTIVE BOOLEAN NOT NULL,
    REASON CHARACTER VARYING(255) NOT NULL,
    TYPE CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT TICKET_REASON_PKEY PRIMARY KEY (ID)
);

								
