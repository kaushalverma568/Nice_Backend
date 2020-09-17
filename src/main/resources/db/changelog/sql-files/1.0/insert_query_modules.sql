/**
 * DashBoard
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Dashboard','Dashboard',true,true);
/**
 * Master
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'City','Master',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Pincode','Master',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Cuisine','Master',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Subscription Plan','Master',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Business Category','Master',true,false);
/**
 * Customer
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Customer','Customer',true,true);
/**
 * Products
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Category', 'Products',false,false);	
	INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Sub Category', 'Products',false,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'UOM', 'Products',false,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Product Attribute', 'Products',false,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Product Toppings', 'Products',false,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Product Addons','Products',false,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Product Extras', 'Products',false,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Product List', 'Products',true,false);	
/**
 * Orders
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Orders','Orders',true,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Delivery Logs','Orders',true,false);
/**
 * Vendor
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Vendor','Vendors',true,true);
/**
 * Payout
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Delivery Boy Payout','Payout',true,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Vendor Payout','Payout',true,false);
/**
 * Delivery Boys
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Delivery Boy', 'Delivery Boys',true,true);
/**
 * Inventory
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Add Stock', 'Inventory',false,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Stock Summary', 'Inventory',false,false);
/**
 * Accounts
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1,'Payment Transaction','Account',true,false);
/**
 * Role & Permission
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Role-Permission','Role-Permission',false,true);
/**
 * HTML section
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Html Section','Html Section',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Slider Banner','Html Section',true,false);
/**
 * Settings
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Settings','Settings',false,true);
/**
 * Company
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Company','Company',false,true);
/**
 * Users
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Users','Users',false,true);
	
/**
 * Others
 */
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Ticket','Others',true,false);	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Reasons','Others',true,false);	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Rating Question','Others',true,false);	
/**
 * Discount
 */
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, parent_module_name,available_for_new_role,no_parent_module)
	VALUES (true, now(), 1, now(), 1, 'Discount','Discount',false,true);	
