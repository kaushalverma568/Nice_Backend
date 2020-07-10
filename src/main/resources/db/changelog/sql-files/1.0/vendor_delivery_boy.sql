CREATE TABLE IF NOT EXISTS VENDOR
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ACCEPTS CHARACTER VARYING(255),
    APPROX_DELIVERY_TIME CHARACTER VARYING(255),
    AREA CHARACTER VARYING(255) NOT NULL,
    BLOCK CHARACTER VARYING(255) NOT NULL,
    BUILDING CHARACTER VARYING(255) NOT NULL,
    CONTACT_NO CHARACTER VARYING(255) NOT NULL,
    DELIVERY_FEE NUMERIC(10,2),
    DELIVERY_TYPE CHARACTER VARYING(255),
    EMAIL CHARACTER VARYING(255) NOT NULL,
    FIRST_NAME CHARACTER VARYING(255) NOT NULL,
    IS_EMAIL_VERIFIED BOOLEAN NOT NULL,
    IS_ORDER_SERVICE_ENABLE BOOLEAN NOT NULL,
    LAST_NAME CHARACTER VARYING(255) NOT NULL,
    LATITUDE NUMERIC(10,6),
    LONGITUDE NUMERIC(10,6),
    MINIMUM_ORDER_AMT NUMERIC(10,2),
    NO_OF_RATING BIGINT,
    OPENING_HOURS CHARACTER VARYING(255),
    PAYMENT_METHOD CHARACTER VARYING(255),
    PROFILE_PICTURE_NAME CHARACTER VARYING(255),
    PROFILE_PICTURE_ORIGINAL_NAME CHARACTER VARYING(255),
    RATING NUMERIC(10,2),
    STATUS CHARACTER VARYING(255) NOT NULL,
    STORE_NAME CHARACTER VARYING(255) NOT NULL,
    STREET CHARACTER VARYING(255) NOT NULL,
    SUBSCRIPTION_PLAN_END_DATE DATE,
    SUBSCRIPTION_PLAN_START_DATE DATE,
    BUSINESS_CATEGORY_ID BIGINT NOT NULL,
    CITY_ID BIGINT,
    COUNTRY_ID BIGINT,
    PINCODE_ID BIGINT,
    SUBSCRIPTION_PLAN_ID BIGINT,
    CONSTRAINT VENDOR_PKEY PRIMARY KEY (ID),
    CONSTRAINT VENDOR_UKEY UNIQUE (EMAIL),
    CONSTRAINT VENDOR_CITY FOREIGN KEY (CITY_ID)
        REFERENCES CITY (ID) ,
    CONSTRAINT VENDOR_BUSINESS_CATEGORY FOREIGN KEY (BUSINESS_CATEGORY_ID)
        REFERENCES BUSINESS_CATEGORY (ID),
    CONSTRAINT VENDOR_COUNTRY FOREIGN KEY (COUNTRY_ID)
        REFERENCES COUNTRY (ID),
    CONSTRAINT VENDOR_SUBSCRIPTION_PLAN FOREIGN KEY (SUBSCRIPTION_PLAN_ID)
        REFERENCES SUBSCRIPTION_PLAN (ID),
    CONSTRAINT VENDOR_PINCODE FOREIGN KEY (PINCODE_ID)
        REFERENCES PINCODE (ID) 
);

CREATE TABLE IF NOT EXISTS VENDOR_BANK_DETAILS
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ACCOUNT_NAME CHARACTER VARYING(255) NOT NULL,
    ACCOUNT_NUMBER CHARACTER VARYING(255) NOT NULL,
    BANK_NAME CHARACTER VARYING(255) NOT NULL,
    BRANCH_CITY CHARACTER VARYING(255) NOT NULL,
    BRANCH_NAME CHARACTER VARYING(255) NOT NULL,
    KIB_NO CHARACTER VARYING(255) NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    CONSTRAINT VENDOR_BANK_DETAILS_PKEY PRIMARY KEY (ID),
    CONSTRAINT VENDOR_BANK_DETAILS_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID)
);

CREATE TABLE IF NOT EXISTS DELIVERY_BOY
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ACCOUNT_NAME CHARACTER VARYING(255),
    BANK_ACCOUNT_NUMBER CHARACTER VARYING(255),
    BANK_NAME CHARACTER VARYING(255),
    BRANCH_CITY CHARACTER VARYING(255),
    BRANCH_NAME CHARACTER VARYING(255),
    EMAIL CHARACTER VARYING(255) NOT NULL,
    FIRST_NAME CHARACTER VARYING(255) NOT NULL,
   	LAST_NAME CHARACTER VARYING(255) NOT NULL,
    GENDER CHARACTER VARYING(255),
    IS_BUSY BOOLEAN NOT NULL,
    IS_EMAIL_VERIFIED BOOLEAN NOT NULL,
    IS_LOGIN BOOLEAN NOT NULL,
    KIB_NO CHARACTER VARYING(255),
    NO_OF_RATING BIGINT NOT NULL,
    PHONE_NUMBER CHARACTER VARYING(255) NOT NULL,
    PROFILE_PICTURE_NAME CHARACTER VARYING(255),
    PROFILE_PICTURE_ORIGINAL_NAME CHARACTER VARYING(255),
    RATING NUMERIC(10,2) NOT NULL,
    CONSTRAINT DELIVERY_BOY_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_UKEY UNIQUE (EMAIL)
);

CREATE TABLE IF NOT EXISTS DELIVERY_BOY_LOCATION
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    LATITUDE NUMERIC(10,6) NOT NULL,
    LONGITUDE NUMERIC(10,6) NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,
    CONSTRAINT DELIVERY_BOY_LOCATION_PKEY PRIMARY KEY (ID),
    CONSTRAINT DELIVERY_BOY_LOCATION_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID) 
);