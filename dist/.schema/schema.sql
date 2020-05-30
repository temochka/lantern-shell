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

INSERT INTO schema_migrations (version) VALUES (20200530050945);

