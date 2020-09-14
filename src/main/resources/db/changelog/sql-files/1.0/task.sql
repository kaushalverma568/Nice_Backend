CREATE TABLE IF NOT EXISTS TASK
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL, 
	DELIVERY_BOY_ID BIGINT,
	ORDER_ID BIGINT NOT NULL,
	STATUS CHARACTER VARYING(255) NOT NULL,
	TASK_TYPE CHARACTER VARYING(255) NOT NULL,
	PAYMENT_DETAILS_ID BIGINT,
	TOTAL_ORDER_AMOUNT NUMERIC(10,2) NOT NULL,
	DELIVERY_CHARGE NUMERIC(10,2) NOT NULL,
	ADMIN_COMMISSION NUMERIC(10,2) DEFAULT 0,
	VENDOR_PAYABLE_AMT NUMERIC(10,2) DEFAULT 0,
	DELIVERED_DATE TIMESTAMP WITH TIME ZONE,
	ORDER_DELIVERY_TYPE CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT TASK_PKEY PRIMARY KEY (ID),
	CONSTRAINT TASK_DELIVERY_BOY_FK FOREIGN KEY (DELIVERY_BOY_ID) REFERENCES DELIVERY_BOY(ID),
	CONSTRAINT TASK_ORDER_FK FOREIGN KEY (ORDER_ID) REFERENCES ORDERS(ID),
	CONSTRAINT TASK_PAYMENT_FK FOREIGN KEY (PAYMENT_DETAILS_ID) REFERENCES PAYMENT_DETAILS(ID)
);

CREATE TABLE IF NOT EXISTS TASK_HISTORY
(
  ID BIGSERIAL NOT NULL,
  TASK_ID BIGINT NOT NULL,
  DELIVERY_BOY_ID BIGINT,
  STATUS CHARACTER VARYING(20) NOT NULL,
  ORDER_ID BIGINT NOT NULL,
  TASK_TYPE CHARACTER VARYING(255),
  CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
  CREATED_BY NUMERIC(10,0) NOT NULL,
  UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,		
  UPDATED_BY NUMERIC(10,0) NOT NULL,
  ORDER_DELIVERY_TYPE CHARACTER VARYING(255) NOT NULL,
  ACTIVE BOOLEAN NOT NULL DEFAULT TRUE,
	CONSTRAINT TASK_HISTORY_PK PRIMARY KEY(ID),
	CONSTRAINT TASK_HISTORY_TASK_FK FOREIGN KEY (TASK_ID) REFERENCES TASK(ID)
);

CREATE TABLE IF NOT EXISTS CASH_COLLECTION
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITHOUT TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    AMOUNT DOUBLE PRECISION NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,
    ORDER_ID BIGINT NOT NULL,
    TASK_ID BIGINT NOT NULL,
    CONSTRAINT CASH_COLLECTION_PKEY PRIMARY KEY (ID),
    CONSTRAINT CASH_COLLECTION_ORDER FOREIGN KEY (ORDER_ID)
        REFERENCES ORDERS (ID) ,
    CONSTRAINT CASH_COLLECTION_TASK FOREIGN KEY (TASK_ID)
        REFERENCES TASK (ID),
    CONSTRAINT CASH_COLLECTION_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID)
);