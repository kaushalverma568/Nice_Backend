ALTER TABLE COMPANY ADD AREA_ID BIGINT;
ALTER TABLE COMPANY ADD CONSTRAINT COMPANY_AREA FOREIGN KEY (AREA_ID) REFERENCES AREA (ID);