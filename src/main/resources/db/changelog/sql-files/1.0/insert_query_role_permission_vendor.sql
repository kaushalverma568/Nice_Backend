INSERT INTO role(
	 active, created_at, created_by, updated_at, updated_by, description, name)
	VALUES (true, now(), 1, now(), 1, 'He/She has Privileges of restaurant specific activities', 'VENDOR');
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 6, 2);
	
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_export, can_import, can_view, can_view_list, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true, true, true, true, true, 7, 2);

							