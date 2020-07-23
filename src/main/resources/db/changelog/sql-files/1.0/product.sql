CREATE TABLE IF NOT EXISTS CATEGORY
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    IMAGE CHARACTER VARYING(255),
    IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    NAME CHARACTER VARYING(255) NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    CONSTRAINT CATEGORY_PKEY PRIMARY KEY (ID),
    CONSTRAINT CATEGORY_VENDOR
  		FOREIGN KEY (VENDOR_ID) REFERENCES VENDOR
);

CREATE TABLE IF NOT EXISTS SUB_CATEGORY
(
    ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    IMAGE CHARACTER VARYING(255),
    IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    NAME CHARACTER VARYING(255) NOT NULL,
    CATEGORY_ID BIGINT NOT NULL,
    CONSTRAINT SUB_CATEGORY_PKEY PRIMARY KEY (ID),
    CONSTRAINT SUB_CATEGORY_CATEGORY FOREIGN KEY (CATEGORY_ID)
        REFERENCES CATEGORY (ID)
 );

CREATE TABLE IF NOT EXISTS PRODUCT
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
    CATEGORY_ID BIGINT NOT NULL,
    DESCRIPTION CHARACTER VARYING(255) NOT NULL,
    IMAGE CHARACTER VARYING(255),
    IMAGE_ORIGINAL_NAME CHARACTER VARYING(255), 
	DETAIL_IMAGE CHARACTER VARYING(255),
    DETAIL_IMAGE_ORIGINAL_NAME CHARACTER VARYING(255),
    NAME CHARACTER VARYING(255) NOT NULL,
    SUBCATEGORY_ID BIGINT,
    BRAND_ID BIGINT,
    VENDOR_ID BIGINT NOT NULL,
    CUISINE_ID BIGINT,
    PRODUCT_AVAILABLE BOOLEAN DEFAULT FALSE,
    COMBO BOOLEAN DEFAULT FALSE,
    RATING NUMERIC(7,2) NOT NULL DEFAULT 0,
	NO_OF_RATING BIGINT NOT NULL DEFAULT 0,
	PRODUCT_FOOD_TYPE INT NOT NULL,
	DISCOUNT_ID BIGINT,
    CONSTRAINT PRODUCT_PKEY PRIMARY KEY (ID),
    CONSTRAINT PRODUCT_CATEGORY FOREIGN KEY (CATEGORY_ID)
        REFERENCES CATEGORY (ID),
    CONSTRAINT PRODUCT_SUBCATEGORY FOREIGN KEY (SUBCATEGORY_ID)
        REFERENCES SUB_CATEGORY (ID),
    CONSTRAINT PRODUCT_VARIANT_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID), 
	CONSTRAINT PRODUCT_BRAND FOREIGN KEY (BRAND_ID)
        REFERENCES BRAND (ID),
    CONSTRAINT PRODUCT_CUISINE FOREIGN KEY (CUISINE_ID)
        REFERENCES CUISINE (ID)
);

CREATE TABLE IF NOT EXISTS PRODUCT_VARIANT
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
    DISCOUNTED_RATE NUMERIC(10,2),
    RATE NUMERIC(10,2) NOT NULL,
    SKU CHARACTER VARYING(255),
    PRODUCT_ID BIGINT NOT NULL,
    UOM_ID BIGINT NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    PRODUCT_AVAILABLE BOOLEAN,
   
    CONSTRAINT PRODUCT_VARIANT_PKEY PRIMARY KEY (ID),
    CONSTRAINT PRODUCT_VARIANT_PRODUCT FOREIGN KEY (PRODUCT_ID)
        REFERENCES PRODUCT (ID),
    CONSTRAINT PRODUCT_VARIANT_UOM FOREIGN KEY (UOM_ID)
        REFERENCES UOM (ID),
    CONSTRAINT PRODUCT_VARIANT_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
    CONSTRAINT PRODUCT_VARIANT_UNIQUE UNIQUE(PRODUCT_ID, VENDOR_ID, UOM_ID)
);

CREATE TABLE IF NOT EXISTS PRODUCT_RATE_UPDATE_HISTORY
(
    ID BIGSERIAL NOT NULL,
    NEW_DISCOUNTED_RATE NUMERIC(10,2),
    NEW_RATE NUMERIC(10,2),
    OLD_DISCOUNTED_RATE NUMERIC(10,2),
    OLD_RATE NUMERIC(10,2),
    PRODUCT_VARIANT_ID BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
 	CONSTRAINT PRODUCT_RATE_UPDATE_HISTORY_PKEY PRIMARY KEY (ID)
);

 CREATE TABLE IF NOT EXISTS TOPPING
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
	NAME CHARACTER VARYING(255) NOT NULL,
    DESCRIPTION CHARACTER VARYING(255),
    VENDOR_ID BIGINT NOT NULL,
    PRODUCT_FOOD_TYPE CHARACTER VARYING(255),
    CONSTRAINT TOPPING_PKEY PRIMARY KEY (ID),
   	CONSTRAINT TOPPING_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
	CONSTRAINT TOPPING_NAME_UNIQUE UNIQUE(VENDOR_ID, NAME)
);

CREATE TABLE IF NOT EXISTS PRODUCT_TOPPING
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
    RATE NUMERIC(10,2) NOT NULL,
    DISCOUNTED_RATE NUMERIC(10,2),
    PRODUCT_VARIANT_ID BIGINT NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    TOPPING_ID BIGINT NOT NULL,
    CONSTRAINT PRODUCT_TOPPING_PKEY PRIMARY KEY (ID),
    CONSTRAINT PRODUCT_TOPPING_VARIANT FOREIGN KEY (PRODUCT_VARIANT_ID)
        REFERENCES PRODUCT_VARIANT (ID), 
    CONSTRAINT PRODUCT_TOPPING_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
    CONSTRAINT PRODUCT_TOPPING_TOPPING FOREIGN KEY (TOPPING_ID) REFERENCES TOPPING (ID),
	CONSTRAINT VENDOR_TOPPING_NAME_UNIQUE UNIQUE(VENDOR_ID, TOPPING_ID, PRODUCT_VARIANT_ID)
);

CREATE TABLE IF NOT EXISTS ADDONS
(
	ID BIGSERIAL NOT NULL,
	ACTIVE BOOLEAN NOT NULL,
	CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
	CREATED_BY BIGINT NOT NULL,
	UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
	UPDATED_BY BIGINT NOT NULL,
	NAME CHARACTER VARYING(255) NOT NULL,
	DESCRIPTION CHARACTER VARYING(255),
	VENDOR_ID BIGINT NOT NULL,
	CONSTRAINT ADDONS_PKEY PRIMARY KEY (ID),
	CONSTRAINT ADDONS_VENDOR FOREIGN KEY (VENDOR_ID)
		REFERENCES VENDOR (ID),
	CONSTRAINT VENDOR_ADDONS_NAME_UNIQUE UNIQUE(VENDOR_ID, NAME)
);

CREATE TABLE IF NOT EXISTS PRODUCT_ADDONS
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,  
    RATE NUMERIC(10,2) NOT NULL,
    DISCOUNTED_RATE NUMERIC(10,2),
    PRODUCT_VARIANT_ID BIGINT NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    PRODUCT_FOOD_TYPE CHARACTER VARYING(255),
    ADDONS_ID BIGINT NOT NULL,
    CONSTRAINT PRODUCT_ADDONS_PKEY PRIMARY KEY (ID),
    CONSTRAINT PRODUCT_ADDONS_VARIANT FOREIGN KEY (PRODUCT_VARIANT_ID)
        REFERENCES PRODUCT_VARIANT (ID), 
    CONSTRAINT PRODUCT_ADDONS_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
    CONSTRAINT PRODUCT_ADDONS_ADDONS FOREIGN KEY (ADDONS_ID)
		REFERENCES ADDONS (ID),
	CONSTRAINT PRODUCT_ADDONS_VENDOR_NAME_UNIQUE UNIQUE(VENDOR_ID, ADDONS_ID, PRODUCT_VARIANT_ID)
);

	
CREATE TABLE IF NOT EXISTS PRODUCT_ATTRIBUTE
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NAME CHARACTER VARYING(255) NOT NULL,
	VENDOR_ID BIGINT NOT NULL, 
    DESCRIPTION CHARACTER VARYING(255),
    CONSTRAINT PRODUCT_ATTRIBUTE_PKEY PRIMARY KEY (ID),
	CONSTRAINT PRODUCT_ATTR_VENDOR_FK FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
    CONSTRAINT PRODUCT_ATTR_VENDOR_NAME_UNQ UNIQUE(VENDOR_ID, NAME)
);

CREATE TABLE IF NOT EXISTS PRODUCT_ATTRIBUTE_VALUES(
 
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    PRODUCT_ATTRIBUTE_ID BIGINT NOT NULL, 
    ATTRIBUTE_VALUE CHARACTER VARYING(255) NOT NULL,
    PRODUCT_VARIANT_ID BIGINT NOT NULL, 
    DESCRIPTION CHARACTER VARYING(255),
    RATE  NUMERIC(10,2) NOT NULL,
    DISCOUNTED_RATE  NUMERIC(10,2),
    CONSTRAINT PRODUCT_ATTRIBUTE_VALUES_PKEY PRIMARY KEY (ID),
	CONSTRAINT PRODUCT_ATTR_PRODUCT_ATTR_VALUE_FK FOREIGN KEY (PRODUCT_ATTRIBUTE_ID)
        REFERENCES PRODUCT_ATTRIBUTE (ID),
	CONSTRAINT PRODUCT_PRODUCT_ATTR_VALUE_FK FOREIGN KEY (PRODUCT_VARIANT_ID)
        REFERENCES PRODUCT_VARIANT (ID),
    CONSTRAINT PROD_ATTR_VAL_VARIANT_ATTR_VAL_UNQ UNIQUE(PRODUCT_ATTRIBUTE_ID, ATTRIBUTE_VALUE, PRODUCT_VARIANT_ID)
);

CREATE TABLE PRODUCT_EXTRAS_MASTER
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    DESCRIPTION CHARACTER VARYING(255),
    NAME CHARACTER VARYING(255) NOT NULL,
    RATE DOUBLE PRECISION NOT NULL,
    VENDOR_ID BIGINT NOT NULL,
    CONSTRAINT PRODUCT_EXTRAS_MASTER_PKEY PRIMARY KEY (ID),
    CONSTRAINT PRODUCT_EXTRAS_MASTER_VENDOR FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID)
);

CREATE TABLE IF NOT EXISTS PRODUCT_EXTRAS (

    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
	VENDOR_ID BIGINT NOT NULL, 
	PRODUCT_ID BIGINT,
	RATE  NUMERIC(10,2) NOT NULL,
	DISCOUNTED_RATE  NUMERIC(10,2),
	PRODUCT_EXTRAS_MASTER_ID BIGINT,
	CONSTRAINT PRODUCT_EXTRAS_PKEY PRIMARY KEY (ID),
	CONSTRAINT PRODUCT_EXTRAS_VENDOR_FK FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
	CONSTRAINT PRODUCT_EXTRAS_PRODUCT_FK FOREIGN KEY (PRODUCT_ID)
        REFERENCES PRODUCT (ID),
    CONSTRAINT PRODUCT_EXTRAS_MASTER_PRODUCT_EXTRAS_FK FOREIGN KEY (PRODUCT_EXTRAS_MASTER_ID)
        REFERENCES PRODUCT_EXTRAS_MASTER (ID),
    CONSTRAINT PRODUCT_EX_PROD_NAME_UNQ UNIQUE(PRODUCT_ID, PRODUCT_EXTRAS_MASTER_ID)      
 );
 