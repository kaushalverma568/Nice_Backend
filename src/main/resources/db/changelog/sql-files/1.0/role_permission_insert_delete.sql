DELETE FROM PERMISSION;
DELETE FROM MODULES;

ALTER TABLE IF EXISTS MODULES DROP COLUMN USER_ROLE;
ALTER TABLE IF EXISTS USERS DROP COLUMN ROLE;
ALTER TABLE IF EXISTS USER_LOGIN DROP COLUMN ROLE;
ALTER TABLE IF EXISTS PERMISSION DROP COLUMN CAN_VIEW_LIST;
ALTER TABLE IF EXISTS PERMISSION DROP COLUMN CAN_IMPORT;
ALTER TABLE IF EXISTS PERMISSION DROP COLUMN CAN_EXPORT;
ALTER TABLE IF EXISTS MODULES ADD COLUMN AVAILABLE_FOR_NEW_ROLE BOOLEAN DEFAULT FALSE;
ALTER TABLE IF EXISTS MODULES ADD COLUMN PARENT_MODULE_NAME CHARACTER VARYING(255) DEFAULT ' ' NOT NULL;
ALTER TABLE IF EXISTS PERMISSION ADD COLUMN SIDE_BAR BOOLEAN DEFAULT FALSE NOT NULL;
ALTER TABLE IF EXISTS ROLE ADD COLUMN IS_DEFAULT BOOLEAN DEFAULT TRUE;
ALTER TABLE IF EXISTS USER_LOGIN ADD COLUMN ROLE_ID BIGINT DEFAULT 1 NOT NULL;
ALTER TABLE IF EXISTS USERS ADD COLUMN ROLE_ID BIGINT NOT NULL;
ALTER TABLE IF EXISTS USER_LOGIN ADD CONSTRAINT FKRPHAI2LLEJJMGCGSPOW42PE7J FOREIGN KEY (ROLE_ID) REFERENCES ROLE;
ALTER TABLE IF EXISTS USERS ADD CONSTRAINT FK4QU1GR772NNF6VE5AF002RWYA FOREIGN KEY (ROLE_ID) REFERENCES ROLE;


UPDATE role SET name='CUSTOMER',description=' ' WHERE id='2';

INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, ' ', 'DELIVERY_BOY', true);

INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name, is_default)
	VALUES (true, now(), 1, now(), 1, 'He/She has Privileges of restaurant specific activities', 'VENDOR', true);

INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (1,true, now(), 1, now(), 1, 'Country','Master',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (2,true, now(), 1, now(), 1, 'State','Master',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (3,true, now(), 1, now(), 1, 'City','Master',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (4,true, now(), 1, now(), 1, 'Pincode','Master',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (5,true, now(), 1, now(), 1, 'Cuisine','Master',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (6,true, now(), 1, now(), 1, 'Subscription Plan','Master',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (7,true, now(), 1, now(), 1, 'Business Category','Master',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (8,true, now(), 1, now(), 1, 'Settings','Settings',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (9,true, now(), 1, now(), 1, 'Company','Company',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (10,true, now(), 1, now(), 1, 'Html Section','Html Section',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (11,true, now(), 1, now(), 1, 'Slider Banner','Html Section',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (12,true, now(), 1, now(), 1, 'Role-Permission','Role-Permission',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (13,true, now(), 1, now(), 1, 'Customer','Customer',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (14,true, now(), 1, now(), 1, 'Others','Ticket',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (15,true, now(), 1, now(), 1, 'Others','Reasons',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (16,true, now(), 1, now(), 1, 'Others','Rating Question',true);
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (17,true, now(), 1, now(), 1, 'Vendors','Vendor',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (18,true, now(), 1, now(), 1, 'Products','Category',false);	
	INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (19,true, now(), 1, now(), 1, 'Products','Sub Category',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (20,true, now(), 1, now(), 1, 'Products','UOM',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (21,true, now(), 1, now(), 1, 'Products','Product Attribute',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (22,true, now(), 1, now(), 1, 'Products','Product Toppings',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (23,true, now(), 1, now(), 1, 'Products','Product Addons',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (24,true, now(), 1, now(), 1, 'Products','Product Extras',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (25,true, now(), 1, now(), 1, 'Products','Product List',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (26,true, now(), 1, now(), 1, 'Inventory','Add Stock',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (27,true, now(), 1, now(), 1, 'Inventory','Stock Summary',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (28,true, now(), 1, now(), 1, 'Orders','Orders',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (29,true, now(), 1, now(), 1, 'Orders','Delivery Logs',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (30,true, now(), 1, now(), 1, 'Delivery Boys','Delivery Boy',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (31,true, now(), 1, now(), 1, 'Users','Users',false);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (32,true, now(), 1, now(), 1, 'Payout','Delivery Boy Payout',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (33,true, now(), 1, now(), 1, 'Payout','Vendor Payout',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (34,true, now(), 1, now(), 1, 'Account','Payment Transaction',true);	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (35,true, now(), 1, now(), 1, 'Discount','Discount',false);	

	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 1, 1,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 2, 1,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 3, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 4, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 5, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 6, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 7, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 8, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 9, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 10, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 11, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 12, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, true, false,true, 13, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, true,true, 14, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 15, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 16, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, true, true,true, 17, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 28, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 29, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, true, true,true, 30, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 31, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 32, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 33, 1,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 34, 1,true);

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 13, 2,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 14, 2,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 28, 2,false);

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 13, 3,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 14, 3,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 30, 3,false);

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 6, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 7, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 13, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 14, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 17, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 18, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 19, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 20, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 21, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 22, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 23, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 24, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 25, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 26, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 27, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, true,true, 28, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 30, 4,false);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 33, 4,true);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id, side_bar)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 35, 4,true);
	
	
	