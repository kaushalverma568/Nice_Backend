DELETE FROM PERMISSION;
DELETE FROM MODULES;
ALTER TABLE IF EXISTS modules ADD COLUMN USER_ROLE CHARACTER VARYING(255) NOT NULL default ' ';
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Role & permissions','SUPER_ADMIN');
	
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Country','SUPER_ADMIN');

INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'State','SUPER_ADMIN');
	
INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'City','SUPER_ADMIN');

INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Pincode','SUPER_ADMIN');
		
INSERT INTO modules(
	active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Category','VENDOR');

INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Sub Category','VENDOR');

INSERT INTO modules(
	 active, created_at, created_by, updated_at, updated_by, name, user_role)
	VALUES (true, now(), 1, now(), 1, 'Brand','SUPER_ADMIN');

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 1, 1);

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 2, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 3, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 4, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 5, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 6, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 7, 1);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 8, 1);
		
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 6, 2);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 7, 2);
	