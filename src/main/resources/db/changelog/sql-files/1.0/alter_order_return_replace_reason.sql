ALTER TABLE ORDERS ADD COLUMN RETURN_REPLACE_REQUEST_CANCEL_REJECT_DESCRIPTION CHARACTER VARYING(255);
ALTER TABLE ORDERS ADD COLUMN RETURN_REPLACE_REQUEST_CANCEL_REJECT_REASON_ID BIGINT;
ALTER TABLE ORDERS ADD CONSTRAINT RETURN_REPLACE_REQ_CANC_REASON_ID_FK FOREIGN KEY (RETURN_REPLACE_REQUEST_CANCEL_REJECT_REASON_ID) REFERENCES TICKET_REASON(ID);