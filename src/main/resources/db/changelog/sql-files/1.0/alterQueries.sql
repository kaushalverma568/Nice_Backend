ALTER TABLE IF EXISTS CUSTOMER_ADDRESS ADD COLUMN ADDRESS_OF VARCHAR(255) NOT NULL DEFAULT '';
ALTER TABLE IF EXISTS ONLINE_CART ADD COLUMN DESCRIPTION VARCHAR(255);
ALTER TABLE IF EXISTS ORDERS ADD COLUMN CANCEL_RETURN_REPLACE_DESCRIPTION VARCHAR(255);