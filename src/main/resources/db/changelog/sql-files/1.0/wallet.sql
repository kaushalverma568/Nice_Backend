 ALTER TABLE  IF EXISTS CUSTOMER ADD COLUMN WALLET_AMT NUMERIC(10,2) DEFAULT 0;
 
 CREATE TABLE IF NOT EXISTS WALLET_TRX
 (
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
	PREVIOUS_AMT NUMERIC(10,2) NOT NULL,
	AMOUNT NUMERIC(10,2) NOT NULL,
	CURRENT_AMT NUMERIC(10,2) NOT NULL,
	CUSTOMER_ID BIGINT NOT NULL,
	ORDER_ID BIGINT NOT NULL,
    CONSTRAINT WALLET_TRX_PKEY PRIMARY KEY (ID),
    CONSTRAINT CUSTOMER_WALLET FOREIGN KEY (CUSTOMER_ID)
        REFERENCES CUSTOMER(ID),
	CONSTRAINT ORDER_WALLET FOREIGN KEY (ORDER_ID)
        REFERENCES ORDERS(ID)
);