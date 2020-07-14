ALTER TABLE VENDOR
ADD COLUMN IS_CONTACT_VERIFIED BOOLEAN NOT NULL DEFAULT false;

INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('VENDOR_SUBSCRIPTION_EXPIRE',TRUE, NOW(),1, NOW(), 1);
INSERT INTO SCHEDULER_DETAILS (NAME, ACTIVE, CREATED_AT, CREATED_BY, UPDATED_AT, UPDATED_BY) VALUES ('VENDOR_SUBSCRIPTION_EXPIRE_REMINDER',TRUE, NOW(),1, NOW(), 1);