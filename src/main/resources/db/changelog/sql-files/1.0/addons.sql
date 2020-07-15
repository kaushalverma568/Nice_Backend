ALTER TABLE PRODUCT_ADDONS DROP COLUMN NAME;
ALTER TABLE PRODUCT_ADDONS DROP COLUMN DESCRIPTION;

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


ALTER TABLE PRODUCT_ADDONS ADD COLUMN ADDONS_ID BIGINT NOT NULL;
ALTER TABLE PRODUCT_ADDONS ADD CONSTRAINT PRODUCT_ADDONS_ADDONS FOREIGN KEY (ADDONS_ID)
REFERENCES ADDONS (ID);