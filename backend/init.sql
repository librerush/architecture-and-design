CREATE TABLE student (
  id SERIAL PRIMARY KEY,
  name text NOT NULL,
  password text NOT NULL,
  score int,
  courses int[]
);

CREATE TABLE course (
  id SERIAL PRIMARY KEY,
  name text NOT NULL,
  description text,
  materials text[]
);

CREATE TABLE staff (
  id SERIAL PRIMARY KEY,
  name text NOT NULL,
  password text NOT NULL,
  courses int[]
);

