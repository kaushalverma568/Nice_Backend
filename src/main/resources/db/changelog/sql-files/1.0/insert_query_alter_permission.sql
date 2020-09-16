UPDATE permission SET can_view='false' WHERE modules_id='7' and role_id='4';

/**
* Added for special requirement by client regarding product list can be viewed by super admin
*/

UPDATE modules SET available_for_new_role='true' WHERE name='Product List';

INSERT INTO permission(
	 active, created_at, created_by, updated_at, updated_by, can_add, can_delete, can_edit, can_view, modules_id, role_id)
	VALUES (true, now(), 1, now(), 1, false, false, false,true, 15, 1);
