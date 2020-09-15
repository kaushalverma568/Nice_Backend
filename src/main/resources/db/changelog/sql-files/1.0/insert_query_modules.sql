/**
 * DashBoard
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Dashboard','Dashboard',true);
/**
 * Master
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'City','Master',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Pincode','Master',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Cuisine','Master',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Subscription Plan','Master',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Business Category','Master',true);
/**
 * Customer
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Customer','Customer',true);
/**
 * Products
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Category', 'Products',false);	
	INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Sub Category', 'Products',false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'UOM', 'Products',false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Product Attribute', 'Products',false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Product Toppings', 'Products',false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Product Addons','Products',false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Product Extras', 'Products',false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Product List', 'Products',false);	
/**
 * Orders
 */
NSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Orders','Orders',true);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Delivery Logs','Orders',true);
/**
 * Vendor
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Vendor','Vendors',true);
/**
 * Payout
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Delivery Boy Payout','Payout',true);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Vendor Payout','Payout',true);
/**
 * Delivery Boys
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Delivery Boy', 'Delivery Boys',true);
/**
 * Inventory
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Add Stock', 'Inventory',false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Stock Summary', 'Inventory',false);
/**
 * Accounts
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1,'Payment Transaction','Account',true);
/**
 * Role & Permission
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Role-Permission','Role-Permission',false);
/**
 * HTML section
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Html Section','Html Section',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Slider Banner','Html Section',true);
/**
 * Settings
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Settings','Settings',false);
/**
 * Company
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Company','Company',false);
/**
 * Users
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Users','Users',false);
	
/**
 * Others
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Ticket','Others',true);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Reasons','Others',true);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Rating Question','Others',true);	
/**
 * Discount
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role)
	VALUES (true, now(), 1, now(), 1, 'Discount','Discount',false);	
