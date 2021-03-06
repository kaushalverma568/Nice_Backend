CREATE TABLE IF NOT EXISTS VENDOR_HISTORY
(
    ID BIGSERIAL NOT NULL,
    FIRST_NAME_ENGLISH CHARACTER VARYING(255) NOT NULL,
    FIRST_NAME_ARABIC CHARACTER VARYING(255) NOT NULL,
    LAST_NAME_ENGLISH CHARACTER VARYING(255) NOT NULL,
    LAST_NAME_ARABIC CHARACTER VARYING(255) NOT NULL,
    BLOCK_ENGLISH CHARACTER VARYING(255) NOT NULL,
    BLOCK_ARABIC CHARACTER VARYING(255) NOT NULL,
    BUILDING_ENGLISH CHARACTER VARYING(255) NOT NULL,
    BUILDING_ARABIC CHARACTER VARYING(255) NOT NULL,
    STREET_ENGLISH CHARACTER VARYING(255) NOT NULL,
    STREET_ARABIC CHARACTER VARYING(255) NOT NULL,
    PHONE_NUMBER CHARACTER VARYING(255) NOT NULL,
    DELIVERY_TYPE CHARACTER VARYING(255),
    EMAIL CHARACTER VARYING(255) NOT NULL,
    EMAIL_VERIFIED BOOLEAN NOT NULL,
    PREFERRED_LANGUAGE CHARACTER VARYING(20) NOT NULL,
    IS_ORDER_SERVICE_ENABLE BOOLEAN NOT NULL,
    LATITUDE NUMERIC(10,6),
    LONGITUDE NUMERIC(10,6),
    MINIMUM_ORDER_AMT NUMERIC(10,2),
    NO_OF_RATING BIGINT,
    OPENING_HOURS_FROM TIME WITH TIME ZONE,
    OPENING_HOURS_TO TIME WITH TIME ZONE,
    PAYMENT_METHOD CHARACTER VARYING(255),
    ACCEPTS CHARACTER VARYING(255),
    STORE_NAME_ENGLISH CHARACTER VARYING(255) NOT NULL,
    STORE_NAME_ARABIC CHARACTER VARYING(255) NOT NULL,
    STORE_IMAGE_NAME CHARACTER VARYING(255),
    STORE_IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    FEATURED_IMAGE_NAME CHARACTER VARYING(255),
    FEATURED_IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    STORE_DETAIL_IMAGE_NAME CHARACTER VARYING(255),
    STORE_DETAIL_IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    RATING NUMERIC(10,2),
    STATUS CHARACTER VARYING(255) NOT NULL,
    SUBSCRIPTION_PLAN_END_DATE DATE,
    SUBSCRIPTION_PLAN_START_DATE DATE,
    BUSINESS_CATEGORY_ID BIGINT NOT NULL,
    CITY_ID BIGINT,
    COUNTRY_ID BIGINT,
    SUBSCRIPTION_PLAN_ID BIGINT,
    PHONE_VERIFIED BOOLEAN NOT NULL,
    STORE_PHONE_NUMBER CHARACTER VARYING(256),
    MAX_DAYS_FOR_ACCEPT INTEGER,
    IS_FEATURED BOOLEAN,
    PROFILE_COMPLETED BOOLEAN,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    AREA_ID BIGINT NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    CONSTRAINT VENDOR_HISTORY_PKEY PRIMARY KEY (ID),
    CONSTRAINT VENDOR_HISTORY_CITY FOREIGN KEY (CITY_ID)
        REFERENCES CITY (ID) ,
    CONSTRAINT VENDOR_HISTORY_BUSINESS_CATEGORY FOREIGN KEY (BUSINESS_CATEGORY_ID)
        REFERENCES BUSINESS_CATEGORY (ID),
    CONSTRAINT VENDOR_HISTORY_COUNTRY FOREIGN KEY (COUNTRY_ID)
        REFERENCES COUNTRY (ID),
    CONSTRAINT VENDOR_HISTORY_SUBSCRIPTION_PLAN FOREIGN KEY (SUBSCRIPTION_PLAN_ID)
        REFERENCES SUBSCRIPTION_PLAN (ID), 
    CONSTRAINT VENDOR_HISTORY_AREA FOREIGN KEY (AREA_ID)
        REFERENCES AREA (ID), 
    CONSTRAINT VENDOR_HISTORY_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID)
);