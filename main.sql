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
    date_of_birth
);

create table playerteam(
    id integer primary key,
    player_id,
    team_id,
    date_from,
    date_to
);

create index playerteam_player_id on playerteam(player_id);
create index playerteam_team_id on playerteam(team_id);

create table playerattr(
    id integer primary key,
    player_id,
    date,
    att,
    def,
    vel
);

create index player_attr_player_id on playerattr(player_id);

create table match(
    id integer primary key,
    date,
    division_id,
    season_id,
    team1_id,
    team2_id,
    score_team1,
    score_team2
);

create index match_team1_id on match(team1_id);
create index match_team2_id on match(team2_id);
