ALTER TABLE BRAND RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE BRAND ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE BUSINESS_CATEGORY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE BUSINESS_CATEGORY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE CATEGORY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE CATEGORY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE SUB_CATEGORY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE SUB_CATEGORY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE COUNTRY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE COUNTRY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE CITY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE CITY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE TICKET_REASON RENAME COLUMN REASON TO REASON_ENGLISH;
ALTER TABLE TICKET_REASON ADD COLUMN REASON_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE CUISINE RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE CUISINE ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE STATE RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE STATE ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE SUBSCRIPTION_PLAN RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE SUBSCRIPTION_PLAN ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE SUBSCRIPTION_PLAN RENAME COLUMN DESCRIPTION TO DESCRIPTION_ENGLISH;
ALTER TABLE SUBSCRIPTION_PLAN ADD COLUMN DESCRIPTION_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE UOM RENAME COLUMN MEASUREMENT TO MEASUREMENT_ENGLISH;
ALTER TABLE UOM ADD COLUMN MEASUREMENT_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE UOM RENAME COLUMN UOM_LABEL TO UOM_LABEL_ENGLISH;
ALTER TABLE UOM ADD COLUMN UOM_LABEL_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE COMPANY RENAME COLUMN NAME TO NAME_ENGLISH;
ALTER TABLE COMPANY ADD COLUMN NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE COMPANY RENAME COLUMN COMPANY_ADDRESS TO COMPANY_ADDRESS_ENGLISH;
ALTER TABLE COMPANY ADD COLUMN COMPANY_ADDRESS_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE USERS RENAME COLUMN FIRST_NAME TO FIRST_NAME_ENGLISH;
ALTER TABLE USERS ADD COLUMN FIRST_NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE USERS RENAME COLUMN LAST_NAME TO LAST_NAME_ENGLISH;
ALTER TABLE USERS ADD COLUMN LAST_NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN FIRST_NAME TO FIRST_NAME_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN FIRST_NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN LAST_NAME TO LAST_NAME_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN LAST_NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN STORE_NAME TO STORE_NAME_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN STORE_NAME_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN BUILDING TO BUILDING_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN BUILDING_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN BLOCK TO BLOCK_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN BLOCK_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN STREET TO STREET_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN STREET_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR RENAME COLUMN AREA TO AREA_ENGLISH;
ALTER TABLE VENDOR ADD COLUMN AREA_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

ALTER TABLE VENDOR_BANK_DETAILS RENAME COLUMN BANK_NAME TO BANK_NAME_ENGLISH;
ALTER TABLE VENDOR_BANK_DETAILS RENAME COLUMN BRANCH_NAME TO BRANCH_NAME_ENGLISH;
ALTER TABLE VENDOR_BANK_DETAILS RENAME COLUMN ACCOUNT_NAME TO ACCOUNT_NAME_ENGLISH;
ALTER TABLE VENDOR_BANK_DETAILS RENAME COLUMN BRANCH_CITY TO BRANCH_CITY_ENGLISH;

ALTER TABLE VENDOR_BANK_DETAILS ADD COLUMN BANK_NAME_ARABIC CHARACTER VARYING(255);
ALTER TABLE VENDOR_BANK_DETAILS ADD COLUMN BRANCH_NAME_ARABIC CHARACTER VARYING(255);
ALTER TABLE VENDOR_BANK_DETAILS ADD COLUMN ACCOUNT_NAME_ARABIC CHARACTER VARYING(255);
ALTER TABLE VENDOR_BANK_DETAILS ADD COLUMN BRANCH_CITY_ARABIC CHARACTER VARYING(255);

ALTER TABLE HTML_SECTION RENAME COLUMN SECTION_VALUE TO SECTION_VALUE_ENGLISH;
ALTER TABLE HTML_SECTION ADD COLUMN SECTION_VALUE_ARABIC CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;

UPDATE BUSINESS_CATEGORY SET NAME_ARABIC = 'توصيل طلبات الطعام' WHERE NAME_ENGLISH = 'Food Delivery';

