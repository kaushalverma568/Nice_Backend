INSERT INTO OAUTH_CLIENT_DETAILS(CLIENT_ID, RESOURCE_IDS, CLIENT_SECRET, SCOPE, AUTHORIZED_GRANT_TYPES, 
            WEB_SERVER_REDIRECT_URI, AUTHORITIES, ACCESS_TOKEN_VALIDITY,REFRESH_TOKEN_VALIDITY, ADDITIONAL_INFORMATION, AUTOAPPROVE)
    VALUES ('kody-client', 'resource_id', '$2a$12$ngzfuVDXy2XhNaGtqFlGF.91JsXehU.WChE18HY.yJBeCjtHHe8EW', 'trust,read,user_info,write',
    'refresh_token,password', '', 'ROLE_ADMIN', 30000, 40000, '{"web_server_redirect_uri":"","additional_information":""}', true);
   
INSERT INTO USER_LOGIN(
	ENTITY_ID, ENTITY_TYPE, EMAIL, PASSWORD, ROLE, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES (null,null,'kodytest.2020@gmail.com', '$2a$12$WMEtIan7v/2U8b2pnCRHb.zm/TH488DlUtZzYo0bSOyfBZT.yaZRa','SUPER_ADMIN',true, now(), 1,now(), 1);   
	

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SEND_SMS', false, false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SEND_EMAIL', false, false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('PAYMENT_GATEWAY_USER_NAME', PGP_SYM_ENCRYPT('default', 'kody_encryption_key'),true, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('PAYMENT_GATEWAY_SECRET', PGP_SYM_ENCRYPT('default', 'kody_encryption_key'), true, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('SMS_API_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('LOGIN_FACEBOOK_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('LOGIN_GOOGLE_KEY', 'default', false, true, now(), 1, now(), 1);

INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('GOOGLE_MAP_KEY', 'DEFAULT', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('VENDOR_COMMISION_RATE', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_REPLACE_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('COMMISION_PER_RETURN_ORDER', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('ORDER_DELIVERY_CHARGE', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
		
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('DAY_MIN_ORDER_DELIVERED', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);
	
INSERT INTO SETTINGS(
	FIELD_NAME, FIELD_VALUE, ENCRYPTED, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY)
	VALUES ('INCENTIVE_AMOUNT_FOR_DAY', '0', FALSE, TRUE, NOW(), 1, NOW(), 1);

INSERT INTO COUNTRY (NAME, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY, ACTIVE) VALUES ('INDIA', NOW(), 1, NOW(), 1, true);
INSERT INTO STATE (NAME, COUNTRY_ID, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY, ACTIVE) VALUES ('GUJARAT', 1,NOW(), 1, NOW(), 1, true);


INSERT INTO company(
	id, active, created_at, created_by, updated_at, updated_by, company_address, company_email, name, phone_number, customer_care_email, gstin, company_image_name, company_image_original_name)
	VALUES (1,true, now(), 1, now(), 1, 'address',' nice@gmail.com', 'Nice Application','1234567890', 'customer_care@gmail.com','qwertyui1234567', 'nice-logo.png','nice-logo.png');


INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('VENDOR_SUBSCRIPTION_EXPIRE',TRUE, NOW(),1, NOW(), 1);
INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('VENDOR_SUBSCRIPTION_EXPIRE_REMINDER',TRUE, NOW(),1, NOW(), 1);
INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('EXPIRE_STOCK_SCHEDULER',TRUE, NOW(),1, NOW(), 1);
INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('ACTIVATE_EXPIRE_DISCOUNT',true, NOW(),1, NOW(), 1);



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
	
INSERT into business_category(active, created_at, created_by, updated_at, updated_by,name,image_name,original_image_name,manage_inventory)
	values(true, now(), 1,now(), 1,'Food Delivery','300x400.png','300x400.png',false);

INSERT into business_category(active, created_at, created_by, updated_at, updated_by,name,image_name,original_image_name,manage_inventory)
	values(true, now(), 1,now(), 1,'Grocery','300x400.png','300x400.png',true);