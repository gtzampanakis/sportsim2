create table if not exists country(
    id integer primary key,
    name
);

create table if not exists division(
    id integer primary key,
    rank,
    country_id
);

create table if not exists team(
    id integer primary key,
    name,
    division_id
);

create table if not exists match(
    id integer primary key,
    date,
    team1_id,
    team2_id,
    result_1,
    result_2
);
