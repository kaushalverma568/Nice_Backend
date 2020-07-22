INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name)
	VALUES (true, now(), 1, now(), 1, 'He/She has higher Privileges', 'SUPER_ADMIN');
	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (1,true, now(), 1, now(), 1, 'Role & permissions');
	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (2,true, now(), 1, now(), 1, 'Country');

INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (3,true, now(), 1, now(), 1, 'State');
	
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (4,true, now(), 1, now(), 1, 'City');

INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (5,true, now(), 1, now(), 1, 'Pincode');
		
INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (6,true, now(), 1, now(), 1, 'Category');

INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (7,true, now(), 1, now(), 1, 'Sub Category');

INSERT INTO modules(
	 id,active, created_at, created_by, updated_at, updated_by, name)
	VALUES (8,true, now(), 1, now(), 1, 'Brand');

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
	
INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name)
	VALUES (true, now(), 1, now(), 1, 'He/She has Privileges of restaurant specific activities', 'VENDOR');
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 6, 2);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 7, 2);

							   
