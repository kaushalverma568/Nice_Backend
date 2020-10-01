/**
* super admin can add vendor
*/

UPDATE permission SET can_add='true' WHERE role_id='1' and modules_id='18';