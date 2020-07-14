DROP TABLE IF EXISTS ORDER_ITEM_RATING ; 
DROP TABLE IF EXISTS ORDER_RATING ;


CREATE TABLE IF NOT EXISTS ORDER_RATING
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
	ORDER_ID BIGINT NOT NULL,
	VENDOR_ID BIGINT NOT NULL,
	DELIVERY_BOY_ID BIGINT,
	QUESTION1_RATING    NUMERIC(7,2),
    QUESTION2_RATING    NUMERIC(7,2),
	QUESTION3_RATING    NUMERIC(7,2),
	QUESTION4_RATING    NUMERIC(7,2),
	QUESTION5_RATING    NUMERIC(7,2),
	DELIVERY_BOY_RATING    NUMERIC(7,2),
	VENDOR_RATING      NUMERIC(7,2),
	AVG_ORDER_RATING   NUMERIC(7,2),
	REVIEW CHARACTER VARYING(255),
    CONSTRAINT ORDER_RATING_PKEY PRIMARY KEY (ID),
	CONSTRAINT ORDER_RATING_VENDOR_fK FOREIGN KEY (VENDOR_ID)
        REFERENCES VENDOR (ID),
	CONSTRAINT ORDER_RATING_BOY_fK FOREIGN KEY (DELIVERY_BOY_ID)
        REFERENCES DELIVERY_BOY (ID)
);

CREATE TABLE IF NOT EXISTS RATING_QUESTION
(
    ID BIGSERIAL NOT NULL,
    ACTIVE BOOLEAN NOT NULL,
    CREATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    CREATED_BY BIGINT NOT NULL,
    UPDATED_AT TIMESTAMP WITH TIME ZONE NOT NULL,
    UPDATED_BY BIGINT NOT NULL,
	QUESTION CHARACTER VARYING(255) NOT NULL,
    TYPE CHARACTER VARYING(255) NOT NULL,
    CONSTRAINT RATING_QUESTION_PKEY PRIMARY KEY (ID)
);



INSERT INTO rating_question(
	 active, created_at, created_by, updated_at, updated_by, question, type)
	VALUES ( true, now(), 1,now(), 1, 'give rate for order packing' , 'Vendor');
	
INSERT INTO rating_question(
	 active, created_at, created_by, updated_at, updated_by, question, type)
	VALUES ( true, now(), 1,now(), 1, 'give rate for food Quality' , 'Vendor');
	
INSERT INTO rating_question(
	 active, created_at, created_by, updated_at, updated_by, question, type)
	VALUES ( true, now(), 1,now(), 1, 'give rate for value of money' , 'Vendor');
	
INSERT INTO rating_question(
	 active, created_at, created_by, updated_at, updated_by, question, type)
	VALUES ( true, now(), 1,now(), 1, 'give rate for Delivery Boy' , 'DeliveryBoy');
	
INSERT INTO rating_question(
	 active, created_at, created_by, updated_at, updated_by, question, type)
	VALUES ( true, now(), 1,now(), 1, 'give rate for Delivery Timing' , 'DeliveryBoy');
	
	
	
