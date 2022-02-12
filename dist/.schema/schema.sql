CREATE TABLE apiCredentials (
  name string NOT NULL PRIMARY KEY,
  value string NOT NULL
);

CREATE TABLE notes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  body TEXT NOT NULL,
  createdAt DATETIME NOT NULL,
  updatedAt DATETIME NOT NULL
);

CREATE TABLE oxfordDictionaryApiCredentials (
  id string NOT NULL PRIMARY KEY,
  key string NOT NULL
);

CREATE TABLE schema_migrations (
                "version" INTEGER PRIMARY KEY NOT NULL
            );

CREATE TABLE scripts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  code TEXT NOT NULL,
  input TEXT NOT NULL,
  created_at DATETIME NOT NULL,
  updated_at DATETIME NOT NULL
);

INSERT INTO schema_migrations (version) VALUES (20220201044423);

