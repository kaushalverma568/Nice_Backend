CREATE TABLE IF NOT EXISTS PAYMENT_DETAILS
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
    NO_OF_ORDERS INTEGER NOT NULL,
    PAID_ON TIMESTAMP WITH TIME ZONE NOT NULL,
    PAYMENT_AMOUNT NUMERIC(10,2) NOT NULL,
    TRANSACTION_NO CHARACTER VARYING(255) NOT NULL,
    DELIVERY_BOY_ID BIGINT NOT NULL,
    CONSTRAINT PAYMENT_DETAILS_PKEY PRIMARY KEY (ID),
    CONSTRAINT PAYMENT_DETAILS_TRANSACTION_NO_UNIQUE UNIQUE (TRANSACTION_NO),
    CONSTRAINT PAYMENT_DETAILS_DELIVERY_BOY FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID) 
);