
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
   LATITUDE NUMERIC(10,6),
   LONGITUDE NUMERIC(10,6),
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
