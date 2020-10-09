UPDATE modules SET name='Area' where name='Pincode';
UPDATE modules SET available_for_new_role='false' where name='City';
UPDATE permission SET can_view_sidebar='false' WHERE role_id='1' and modules_id='2';