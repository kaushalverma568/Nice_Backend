ALTER TABLE CITY DROP COLUMN LONGITUDE;
ALTER TABLE CITY DROP COLUMN LATITUDE;
ALTER TABLE CITY DROP COLUMN IS_DEFAULT;
ALTER TABLE CUSTOMER_ADDRESS DROP COLUMN PINCODE_ID;
ALTER TABLE VENDOR DROP COLUMN PINCODE_ID;
ALTER TABLE ORDERS DROP COLUMN PINCODE_ID;
ALTER TABLE ONLINE_CART DROP COLUMN PINCODE_ID;
ALTER TABLE CUSTOMER_ADDRESS DROP COLUMN AREA;
ALTER TABLE VENDOR DROP COLUMN AREA_ENGLISH;
ALTER TABLE VENDOR DROP COLUMN AREA_ARABIC;
DROP TABLE PINCODE_HISTORY;
DROP TABLE PINCODE;