ALTER TABLE stock_details 
    ALTER COLUMN  lot_no TYPE numeric(10,2),
    ALTER COLUMN  available TYPE numeric(10,2),
    ALTER COLUMN   reserved TYPE numeric(10,2),
    ALTER COLUMN   delivered TYPE numeric(10,2),
    ALTER COLUMN   replaced TYPE numeric(10,2),
    ALTER COLUMN  returned TYPE numeric(10,2),
    ALTER COLUMN   expired TYPE numeric(10,2);