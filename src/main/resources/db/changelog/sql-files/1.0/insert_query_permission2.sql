/**
* Dashboard for vendor
*/
INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, true, true, true,true, 1, 4);
