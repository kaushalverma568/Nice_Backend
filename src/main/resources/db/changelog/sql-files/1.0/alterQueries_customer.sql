ALTER TABLE IF EXISTS CUSTOMER_ADDRESS DROP COLUMN LANDMARK;
ALTER TABLE IF EXISTS CUSTOMER_ADDRESS ADD COLUMN AREA CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;