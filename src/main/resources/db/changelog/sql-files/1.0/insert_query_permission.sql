/**
* Dashboard
*/
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 1, 1);
/**
* Master
*/
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 2, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 3, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 4, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 5, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 6, 1); 
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 5, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true,6, 4);
/**
* Customer
*/
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, true, false,true, 7, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 7, 2);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 7, 3);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 7, 4);

/**
* Products
*/
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 8, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 9, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 10, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 11, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 12, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 13, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 14, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 15, 4);

/**
 * Orders
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 16, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 17, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 16, 2);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, true,true, 16, 4);

/**
 * Vendor
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, true, true ,true, 18, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 18, 4);

/**
 * Payout
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 19, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 20, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 20, 4);

/**
 * Delivery Boys
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, true, true,true, 21, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, true,true, 21, 3);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 21, 4);
 
/**
 * Inventory
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 22, 4);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 23, 4);
 
/**
 * Accounts
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 24, 1);
 
/**
 * Role & Permission
 */
 INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 25, 1);

/**
 * HTML section
 */ 
 INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 26, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 27, 1);

/**
 * Settings
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 28, 1);

/**
 * Company
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 29, 1);

/**
 * Users
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 30, 1);
 
/**
 * Others
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, true,true, 31, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 32, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 33, 1);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 31, 2);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 31, 3);
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, false, false,true, 31, 4);

/**
 * Discount
 */
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 34, 4);
	
	
	