CREATE TABLE apiCredentials (
  name string NOT NULL PRIMARY KEY,
  value string NOT NULL
);

CREATE TABLE oxfordDictionaryApiCredentials (
  id string NOT NULL PRIMARY KEY,
  key string NOT NULL
);

CREATE TABLE schema_migrations (
                "version" INTEGER PRIMARY KEY NOT NULL
            );

INSERT INTO schema_migrations (version) VALUES (20200425194106);

