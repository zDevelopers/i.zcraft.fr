CREATE TABLE images (
	storage_name TEXT NOT NULL,
	storage_path TEXT NOT NULL,
	storage_path_mini TEXT NOT NULL,
	url TEXT NOT NULL,
	url_mini TEXT NOT NULL,
    original_name TEXT NOT NULL,
	uploaded_at INTEGER NOT NULL,
	uploaded_by TEXT NOT NULL,
	deletion_token TEXT NOT NULL,
	expires_at INTEGER NOT NULL,
	deleted INTEGER NOT NULL DEFAULT 0,
	CONSTRAINT images_PK PRIMARY KEY (storage_name,storage_path,storage_path_mini,url,url_mini,deletion_token)
);
