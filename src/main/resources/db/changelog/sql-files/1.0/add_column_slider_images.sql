ALTER TABLE slider_image RENAME COLUMN image_name TO app_image_name;
ALTER TABLE slider_image RENAME COLUMN origional_image_name TO app_original_image_name;
ALTER TABLE slider_image ADD COLUMN web_image_name CHARACTER VARYING(255) NOT NULL;
ALTER TABLE slider_image ADD COLUMN web_original_image_name CHARACTER VARYING(255) NOT NULL;