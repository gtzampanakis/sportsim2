create table keyval(
  id integer primary key,
  key text unique,
  value 
);

create table country(
  id integer primary key,
  name text
);

create table team(
  id integer primary key,
  name text,
  country_id integer
);

create table comp(
  id integer primary key,
  name text,
  country_id integer,
  start_month integer,
  start_day integer,
  start_dow integer
);

create table comp_inst(
  id integer primary key,
  comp_id integer,
  season integer
);

create table comp_inst_team(
  id integer primary key,
  comp_inst_id integer,
  team_id integer
);

create table match(
  id integer primary key,
  comp_inst_id integer,
  matchday integer,
  home_team_id integer,
  away_team_id integer,
  home_score integer,
  away_score integer,
  finished integer
);
