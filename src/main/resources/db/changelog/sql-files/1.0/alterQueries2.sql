ALTER TABLE IF EXISTS TICKET DROP COLUMN EMAIL;
ALTER TABLE IF EXISTS TICKET ADD COLUMN ENTITY_ID BIGINT NOT NULL;