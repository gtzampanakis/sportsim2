create table country(
    id integer primary key,
    name
);

create table division(
    id integer primary key,
    rank,
    country_id
);

create table season(
    id integer primary key,
    year,
    season_start
);

create table team(
    id integer primary key,
    name
);

create table teamdivision(
    id integer primary key,
    team_id,
    division_id,
    season_id
);

create table player(
    id integer primary key,
    name,
    ability_att,
    ability_def,
    velocity
);

create table playerteam(
    id integer primary key,
    player_id,
    team_id,
    date_from,
    date_to
);

create table match(
    id integer primary key,
    date,
    team1_id,
    team2_id,
    score_team1,
    score_team2
);
