CREATE TABLE apiCredentials (
  name string NOT NULL PRIMARY KEY,
  value string NOT NULL
);

CREATE TABLE flashcards
( id integer PRIMARY KEY AUTOINCREMENT
, image_front text
, streak_front integer
, streak_back integer
, due_at_front integer
, title_back text
, interval_front integer
, easiness_factor_front real
, is_prompt_back boolean
, is_prompt_front boolean
, content_front text
, due_at_back integer
, last_study_at_back integer
, content_back text
, last_study_at_front integer
, easiness_factor_back real
, interval_back integer
, created_at integer
, image_back text
, title_front text, is_marked_for_correction boolean);

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

INSERT INTO schema_migrations (version) VALUES (20221105175538);

