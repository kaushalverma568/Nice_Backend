ALTER TABLE CUSTOMER_ADDRESS ADD COLUMN AREA_ID BIGINT;
ALTER TABLE CUSTOMER_ADDRESS ADD CONSTRAINT CUSTOMER_ADDRESS_AREA FOREIGN KEY (AREA_ID) REFERENCES AREA (ID);

ALTER TABLE VENDOR ADD COLUMN AREA_ID BIGINT;
ALTER TABLE VENDOR ADD CONSTRAINT VENDOR_AREA FOREIGN KEY (AREA_ID) REFERENCES AREA (ID);

ALTER TABLE ORDERS ADD COLUMN AREA_ID BIGINT;
ALTER TABLE ORDERS ADD CONSTRAINT ORDERS_AREA FOREIGN KEY (AREA_ID) REFERENCES AREA (ID);

ALTER TABLE ONLINE_CART ADD COLUMN AREA_ID BIGINT;

