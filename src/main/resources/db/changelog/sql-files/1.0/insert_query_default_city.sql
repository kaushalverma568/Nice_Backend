ALTER TABLE IF EXISTS CITY ADD COLUMN IS_DEFAULT BOOLEAN DEFAULT FALSE NOT NULL;
ALTER TABLE IF EXISTS CITY ADD COLUMN LATITUDE NUMERIC(10, 6);
ALTER TABLE IF EXISTS CITY ADD COLUMN LONGITUDE NUMERIC(10, 6);
INSERT INTO city(
	 state_id, name_english, name_arabic, active, created_at, created_by, updated_at, updated_by, is_default, latitude, longitude)
	VALUES ( 1, 'Kuwait City', 'مدينة الكويت‎', true, now(), 1, now(), 1, true, '29.369796', '47.979370');
