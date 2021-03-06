
CREATE TABLE IF NOT EXISTS DELIVERY_BOY
(
    ID BIGSERIAL NOT NULL,
    FIRST_NAME_ENGLISH CHARACTER VARYING(255) NOT NULL,
   	LAST_NAME_ENGLISH CHARACTER VARYING(255) NOT NULL,
    FIRST_NAME_ARABIC CHARACTER VARYING(255) NOT NULL,
   	LAST_NAME_ARABIC CHARACTER VARYING(255) NOT NULL,
    ACCOUNT_NAME_ENGLISH CHARACTER VARYING(255),
    ACCOUNT_NAME_ARABIC CHARACTER VARYING(255),
    BANK_ACCOUNT_NUMBER CHARACTER VARYING(255),
    BANK_NAME_ENGLISH CHARACTER VARYING(255),
    BANK_NAME_ARABIC CHARACTER VARYING(255),
    BRANCH_CITY_ENGLISH CHARACTER VARYING(255),
    BRANCH_CITY_ARABIC CHARACTER VARYING(255),
    BRANCH_NAME_ENGLISH CHARACTER VARYING(255),
    BRANCH_NAME_ARABIC CHARACTER VARYING(255),
    EMAIL CHARACTER VARYING(255) NOT NULL,
    GENDER CHARACTER VARYING(255),
    PREFERRED_LANGUAGE CHARACTER VARYING(20) NOT NULL,
    EMAIL_VERIFIED BOOLEAN NOT NULL,
    KIB_NO CHARACTER VARYING(255),
    NO_OF_RATING BIGINT NOT NULL,
    PHONE_NUMBER CHARACTER VARYING(255) NOT NULL,
    PROFILE_PICTURE_NAME CHARACTER VARYING(255),
    PROFILE_PICTURE_ORIGINAL_NAME CHARACTER VARYING(255),
    RATING NUMERIC(10,2) NOT NULL,
    PHONE_VERIFIED BOOLEAN NOT NULL,
    STATUS VARCHAR(255) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT DELIVERY_BOY_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_UKEY UNIQUE (EMAIL)
);

CREATE TABLE IF NOT EXISTS DELIVERY_BOY_LOCATION
(
    ID BIGSERIAL NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,
    LATITUDE NUMERIC(10,6) NOT NULL,
    LONGITUDE NUMERIC(10,6) NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT DELIVERY_BOY_LOCATION_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_LOCATION_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID) 
);

CREATE TABLE  IF NOT EXISTS DELIVERY_BOY_SEND_NOTIFICATION_HISTORY
(
    ID BIGSERIAL NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,
    ORDER_ID BIGINT,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT DELIVERY_BOY_SEND_NOTIFICATION_HISTORY_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_SEND_NOTIFICATION_HISTORY_DELIVERY_BOY_ID_UNIQUE UNIQUE (DELIVERY_BOY_ID),
    CONSTRAINT DELIVERY_BOY_SEND_NOTIFICATION_HISTORY_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID)
);

CREATE TABLE IF NOT EXISTS PAYMENT_DETAILS
(
    ID BIGSERIAL NOT NULL,
    NO_OF_ORDERS INTEGER NOT NULL,
    PAID_ON TIMESTAMP WITH TIME ZONE NOT NULL,
    PAYMENT_AMOUNT NUMERIC(10,2) NOT NULL,
    TRANSACTION_NO CHARACTER VARYING(255) NOT NULL,
    DELIVERY_BOY_ID BIGINT,
    VENDOR_ID BIGINT,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    CONSTRAINT PAYMENT_DETAILS_PKEY PRIMARY KEY (ID),
    CONSTRAINT PAYMENT_DETAILS_TRANSACTION_NO_UNIQUE UNIQUE (TRANSACTION_NO),
    CONSTRAINT PAYMENT_DETAILS_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID),
    CONSTRAINT VEND_FK FOREIGN KEY (VENDOR_ID) REFERENCES VENDOR(ID)
);

CREATE TABLE IF NOT EXISTS DELIVERY_BOY_CURRENT_STATUS
(
    ID BIGSERIAL NOT NULL,
 	DELIVERY_BOY_ID BIGINT NOT NULL,
    IS_AVAILABLE BOOLEAN NOT NULL,
    IS_BUSY BOOLEAN NOT NULL,
    IS_LOGIN BOOLEAN NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  	CREATED_BY NUMERIC(10,0) NOT NULL,
  	UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  	UPDATED_BY NUMERIC(10,0) NOT NULL,
  	CONSTRAINT DELIVERY_BOY_CURRENT_STATUS_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_CURRENT_STATUS_DELIVERY_BOY_UNIQUE UNIQUE (DELIVERY_BOY_ID),
    CONSTRAINT DELIVERY_BOY_CURRENT_STATUS_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID) 
);
