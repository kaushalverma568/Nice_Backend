CREATE TABLE IF NOT EXISTS ORDERS
(
  ID BIGSERIAL NOT NULL,
  CUSTOMER_ID BIGINT NOT NULL,
  STATUS CHARACTER VARYING(20) NOT NULL,
  PAYMENT_MODE CHARACTER VARYING(20) NOT NULL,
  FIRST_NAME CHARACTER VARYING(255),
  LAST_NAME CHARACTER VARYING(255),
  ADDRESS_ENGLISH TEXT NOT NULL,
  ADDRESS_ARABIC TEXT NOT NULL,
  STATE_ID BIGINT NOT NULL,
  CITY_ID BIGINT NOT NULL,
  PINCODE_ID BIGINT NOT NULL,
  DELIVERY_CHARGE NUMERIC(7,2),
  TOTAL_ORDER_AMT NUMERIC(10,2),
  GROSS_ORDER_AMOUNT NUMERIC(10,2),
  DISCOUNT_AMOUNT NUMERIC(10,2),
  DELIVERY_BOY_ID BIGINT,
  PAYMENT_ID CHARACTER VARYING(255),
  ONLINE_ORDER_ID CHARACTER VARYING(255),
  ONLINE_PAYMENT_TOKEN CHARACTER VARYING(255),
  RETURN_REPLACE_CANCEL_REJECT_REASON_ID
  VENDOR_ID BIGINT NOT NULL,
  RETURN_REPLACE_DELIVERY_BOY_ID BIGINT,
  DESCRIPTION TEXT,
  PHONE_NUMBER CHARACTER VARYING(10) NOT NULL,
  REPLACED BOOLEAN NOT NULL DEFAULT FALSE,
  CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  CREATED_BY NUMERIC(10,0) NOT NULL,
  UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  UPDATED_BY NUMERIC(10,0) NOT NULL,	
  LONGITUDE NUMERIC(10, 6),
  ASSIGNMENT_TRY_COUNT INTEGER DEFAULT 0,
  DELIVERY_TYPE VARCHAR(255) NOT NULL,
  LATITUDE NUMERIC(10, 6),
  NOTIFICATION_TIMER TIME WITH TIME ZONE DEFAULT CURRENT_TIME,
  ADMINISTRATIVE_CHARGE NUMERIC(10,2),
  WALLET_CONTRIBUTION NUMERIC(10,2) DEFAULT 0,
  DISTANCE NUMERIC(6,2),
  CANCEL_RETURN_REPLACE_DESCRIPTION VARCHAR(255),
  ACTIVE BOOLEAN NOT NULL,
	CONSTRAINT ORDER_PK PRIMARY KEY(ID),
	CONSTRAINT ORDER_CUSTOMER_FK FOREIGN KEY (CUSTOMER_ID) REFERENCES CUSTOMER(ID),
	CONSTRAINT ORDER_STATE_FK FOREIGN KEY (STATE_ID) REFERENCES STATE(ID),
	CONSTRAINT ORDER_CITY_FK FOREIGN KEY (CITY_ID) REFERENCES CITY(ID),
	CONSTRAINT ORDER_PINCODE_FK FOREIGN KEY (PINCODE_ID) REFERENCES PINCODE(ID),
	CONSTRAINT ORDER_DELIVERY_BOY_FK FOREIGN KEY (DELIVERY_BOY_ID) REFERENCES DELIVERY_BOY(ID),
	CONSTRAINT ORDER_VENDOR_FK FOREIGN KEY (VENDOR_ID) REFERENCES VENDOR(ID),
	CONSTRAINT ORDER_RETURN_DELIVERY_BOY_FK FOREIGN KEY (RETURN_REPLACE_DELIVERY_BOY_ID) REFERENCES DELIVERY_BOY(ID),
	CONSTRAINT REASON_FK FOREIGN KEY (RETURN_REPLACE_CANCEL_REJECT_REASON_ID) REFERENCES TICKET_REASON(ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_ITEM
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    PRODUCT_VARIANT_ID BIGINT NOT NULL,
    ORDER_ID BIGINT NOT NULL,
    QUANTITY BIGINT NOT NULL,
    UNIT_PRICE NUMERIC(10,2) NOT NULL,
    UNIT_PRICE_AFTER_DISCOUNT NUMERIC(10,2),
    TOTAL_DISCOUNT_AMT NUMERIC(10,2),
    TOTAL_AMT NUMERIC(10,2) NOT NULL,
    AMOUNT_BEFORE_DISCOUNT NUMERIC(10,2),
    CONSTRAINT ORDER_ITEM_PKEY PRIMARY KEY (ID),
    CONSTRAINT ORDER_ITEM_PRODUCT_VARIANT FOREIGN KEY (PRODUCT_VARIANT_ID)
        REFERENCES PRODUCT_VARIANT (ID),
	CONSTRAINT ORDER_ITEM_ORDER FOREIGN KEY (ORDER_ID)
        REFERENCES ORDERS (ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_ADDONS
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ORDER_ITEM_ID BIGINT NOT NULL,
    PRODUCT_ADDONS_ID BIGINT NOT NULL,
    QUANTITY BIGINT NOT NULL,
    AMOUNT NUMERIC(10,2) NOT NULL,
    DISCOUNTED_AMOUNT NUMERIC(10,2),
    CONSTRAINT ORDER_ADDONS_PKEY PRIMARY KEY (ID),
    CONSTRAINT ORDER_ADDONS_ORDER_ITEM FOREIGN KEY (ORDER_ITEM_ID)
        REFERENCES ORDERS_ITEM (ID),
    CONSTRAINT ORDER_ADDONS_PRODUCT_ADDONS FOREIGN KEY (PRODUCT_ADDONS_ID)
        REFERENCES PRODUCT_ADDONS (ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_EXTRAS
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ORDER_ITEM_ID BIGINT NOT NULL,
    PRODUCT_EXTRAS_ID BIGINT NOT NULL,
    QUANTITY BIGINT NOT NULL,
    AMOUNT NUMERIC(10,2) NOT NULL,
    DISCOUNTED_AMOUNT NUMERIC(10,2),
    CONSTRAINT ORDER_EXTRAS_PKEY PRIMARY KEY (ID),
    CONSTRAINT ORDER_EXTRAS_ORDER_ITEM FOREIGN KEY (ORDER_ITEM_ID)
        REFERENCES ORDERS_ITEM (ID),
    CONSTRAINT ORDER_EXTRAS_PRODUCT_ADDONS FOREIGN KEY (PRODUCT_EXTRAS_ID)
        REFERENCES PRODUCT_EXTRAS (ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_PRODUCT_ATTRIBUTE
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
     CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ORDER_ITEM_ID BIGINT NOT NULL,
    PRODUCT_ATTRIBUTE_VALUE_ID BIGINT NOT NULL,
    QUANTITY BIGINT NOT NULL,
    AMOUNT NUMERIC(10,2) NOT NULL,
    DISCOUNTED_AMOUNT NUMERIC(10,2),
    CONSTRAINT ORDER_PROD_ATTR_PKEY PRIMARY KEY (ID),
    CONSTRAINT ORDER_PROD_ATTR_ORDER_ITEM FOREIGN KEY (ORDER_ITEM_ID)
        REFERENCES ORDERS_ITEM (ID),
    CONSTRAINT ORDER_PROD_ATTR_PRODUCT_ATTR_VAL FOREIGN KEY (PRODUCT_ATTRIBUTE_VALUE_ID)
        REFERENCES  PRODUCT_ATTRIBUTE_VALUES (ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_TOPPINGS
(
    ID BIGSERIAL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    ORDER_ITEM_ID BIGINT NOT NULL,
    PRODUCT_TOPPINGS_ID BIGINT NOT NULL,
    QUANTITY BIGINT NOT NULL,
    AMOUNT NUMERIC(10,2) NOT NULL,
    DISCOUNTED_AMOUNT NUMERIC(10,2),
    CONSTRAINT ORDER_TOPPINGS_PKEY PRIMARY KEY (ID),
    CONSTRAINT ORDER_TOPPINGS_ORDER_ITEM FOREIGN KEY (ORDER_ITEM_ID)
        REFERENCES ORDERS_ITEM (ID),
    CONSTRAINT ORDER_ADDONS_PRODUCT_TOPPINGS FOREIGN KEY (PRODUCT_TOPPINGS_ID)
        REFERENCES PRODUCT_TOPPING (ID)
);

CREATE TABLE IF NOT EXISTS ORDERS_STATUS_HISTORY
(
  ID BIGSERIAL NOT NULL,
  ORDER_ID BIGINT NOT NULL,
  STATUS CHARACTER VARYING(20) NOT NULL,
  CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  CREATED_BY NUMERIC(10,0) NOT NULL,
  UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  UPDATED_BY NUMERIC(10,0) NOT NULL,	
  ACTIVE BOOLEAN NOT NULL,
	CONSTRAINT ORDERS_STATUS_HISTORY_PK PRIMARY KEY(ID),
	CONSTRAINT ORDERS_STATUS_HISTORY_ORDER_ID_FK FOREIGN KEY (ORDER_ID) REFERENCES ORDERS(ID),
	CONSTRAINT ORDER_STATUS_UNIQUE UNIQUE(ORDER_ID, STATUS)
);

CREATE TABLE IF NOT EXISTS ORDER_LOCATION
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    LATITUDE NUMERIC(10,6) NOT NULL,
    LONGITUDE NUMERIC(10,6) NOT NULL,
    CUSTOMER_ID BIGINT NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,	
   	ORDER_ID BIGINT NOT NULL,
    CONSTRAINT ORDER_LOCATION_PKEY PRIMARY KEY (ID) 
);

