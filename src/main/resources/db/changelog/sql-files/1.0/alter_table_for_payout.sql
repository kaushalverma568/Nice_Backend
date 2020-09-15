ALTER TABLE PAYMENT_DETAILS ADD COLUMN VENDOR_ID BIGINT;
ALTER TABLE TASK ADD COLUMN VENDOR_ID BIGINT;
ALTER TABLE PAYMENT_DETAILS ALTER COLUMN DELIVERY_BOY_ID DROP NOT NULL;
ALTER TABLE TASK ADD CONSTRAINT VEND_FK FOREIGN KEY (VENDOR_ID) REFERENCES VENDOR(ID);
ALTER TABLE PAYMENT_DETAILS ADD CONSTRAINT VEND_FK FOREIGN KEY (VENDOR_ID) REFERENCES VENDOR(ID);