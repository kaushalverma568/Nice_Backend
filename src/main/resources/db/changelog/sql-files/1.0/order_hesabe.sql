ALTER TABLE ONLINE_CART ADD COLUMN PAYMENT_TOKEN CHARACTER VARYING(255);
ALTER TABLE ONLINE_CART ADD COLUMN PAYMENT_ID CHARACTER VARYING(255);
ALTER TABLE ONLINE_CART ADD COLUMN ADMINISTRATIVE_CHARGE NUMERIC(10,2);
ALTER TABLE ONLINE_CART ADD COLUMN WALLET_CONTRIBUTION NUMERIC(10,2);

ALTER TABLE ORDERS RENAME COLUMN TRANSACTION_ID TO PAYMENT_ID;
ALTER TABLE ORDERS RENAME COLUMN ONLINE_SIGNATURE TO ONLINE_PAYMENT_TOKEN;
ALTER TABLE ORDERS ADD COLUMN ADMINISTRATIVE_CHARGE NUMERIC(10,2);