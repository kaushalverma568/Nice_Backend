INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, 'He/She has higher Privileges', 'SUPER_ADMIN', true);

INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, ' ', 'CUSTOMER', true);

INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, ' ', 'DELIVERY_BOY', true);

INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, 'He/She has Privileges of restaurant specific activities', 'VENDOR', true);
 