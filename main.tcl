#!/usr/bin/tclsh
package require sqlite3

proc logistic_cdf {loc sc x} {
    return [expr {1/(1 + exp(-($x - $loc) / $sc))}]
}

proc rand_binomial {p n} {
    set k 0
    for {set i 0} {$i < $n} {incr i} {
        if {rand() < $p} {
            incr k
        }
    }
    return $k
}

proc rand_logistic {loc sc} {
    set r [expr {rand()}]
    return [expr {$loc + $sc * log10($r/(1-$r))}]
}

proc rand_poisson {loc} {
	set L [expr {exp(-$loc)}]
    set k 0
    set p 1
    while 1 {
        incr k
        set p [expr {$p * rand()}]
        if {$p <= $L} {
            break
        }
    }
    return [expr {$k - 1}]
}

proc calc_division_standings {division_id season_id} {
    set results [list]
    db eval {
        select * from (
            select
            td.team_id,
            (
                select 3 * count(m.id)
                from match m
                where m.team1_id = td.team_id
                and m.division_id = $division_id
                and m.season_id = $season_id
                and m.score_team1 > m.score_team2
            )
                +
            (
                select 1 * count(m.id)
                from match m
                where m.team1_id = td.team_id
                and m.division_id = $division_id
                and m.season_id = $season_id
                and m.score_team1 = m.score_team2
            )
                +
            (
                select 3 * count(m.id)
                from match m
                where m.team2_id = td.team_id
                and m.division_id = $division_id
                and m.season_id = $season_id
                and m.score_team2 > m.score_team1
            )
                +
            (
                select 1 * count(m.id)
                from match m
                where m.team2_id = td.team_id
                and m.division_id = $division_id
                and m.season_id = $season_id
                and m.score_team2 = m.score_team1
            ) points
            from teamdivision td
            where 1=1
            and td.division_id = $division_id
            and td.season_id = $season_id
        ) a
-- TODO: Implement tie-breakers instead of just using the team_id
        order by a.points desc, a.team_id
    } row {
        lappend results [array get row]
    }
    return $results
}

proc main1 {} {
    set n_seconds_per_hour 3600
    set n_seconds_per_day [expr $n_seconds_per_hour * 24]
    set n_seconds_per_week [expr $n_seconds_per_day * 7]
    set n_seconds_per_month [expr $n_seconds_per_day * 30]
    set n_seconds_per_year [expr $n_seconds_per_month * 12]

    set conf(n_countries) 1
    set conf(n_divisions_per_country) 3
    set conf(n_teams_per_division) 10
    set conf(n_players_per_team) 11
    set conf(date_start) 0
    set conf(season_start_year_offset) [expr {$n_seconds_per_month * 7}]
    set conf(date_end) \
        [expr {$conf(date_start) + $conf(season_start_year_offset) + $n_seconds_per_year * 5}]
    set conf(promotion_relegation_enabled) 1

    set fp [open "main.sql" r]
    set db_schema_sql [read $fp]
    close $fp

    db eval $db_schema_sql

    proc new_id {} {
        return [expr {int(rand() * 99999999999)}]
    }

    proc range {n} {
        set r [list]
        for {set i 0} {$i < $n} {incr i} {
            lappend r $i
        }
        return $r
    }

    proc lcycle {l} {
        if {[llength $l] == 0} {
            return $l
        } else {
            set ll [llength $l]
            set lldec [expr {$ll - 1}]
            set lldecdec [expr {$ll - 2}]
            set last_el [lindex $l $lldec]
            set r [concat [list $last_el] [lrange $l 0 $lldecdec]]
            return $r
        }
    }

    proc gen_round_robin {nteams nrounds} {
# https://en.wikipedia.org/wiki/Round-robin_tournament#Circle_method
        set nteamsdec [expr {$nteams - 1}]
        set ndays $nteamsdec
        set nmatches [expr {$nteams / 2}]

        set first_round [list] ;# indices: iday imatch iteam
# The last team is the pivot and the rest are the "rest"
        set rest [range $nteamsdec]
# is_last_home_or_away keeps track of whether the previous match was home or
# away so we can alternate.
        set is_previous_home [lrepeat $nteams 0]
        for {set iday 0} {$iday < $ndays} {incr iday} {
            set all [concat [list $nteamsdec] $rest]
            set top_row [lrange $all 0 [expr {$nmatches - 1}]]
            set bottom_row [lreverse [lrange $all $nmatches $nteamsdec]]
            for {set imatch 0} {$imatch < $nmatches} {incr imatch} {
                set team0 [lindex $top_row $imatch]
                set team1 [lindex $bottom_row $imatch]
                set is_previous_home_for_this_pair [ \
                    list \
                        [lindex $is_previous_home $team0] \
                        [lindex $is_previous_home $team1] \
                ]
                if {$is_previous_home_for_this_pair eq [list 0 0]} {
                    set match [list $team0 $team1]
                } elseif {$is_previous_home_for_this_pair eq [list 0 1]} {
                    set match [list $team0 $team1]
                } elseif {$is_previous_home_for_this_pair eq [list 1 0]} {
                    set match [list $team1 $team0]
                } elseif {$is_previous_home_for_this_pair eq [list 1 1]} {
                    set match [list $team1 $team0]
                } else {
                    error "unexpected"
                }
                if {$match eq [list $team0 $team1]} {
                    lset is_previous_home $team0 1
                    lset is_previous_home $team1 0
                } elseif {$match eq [list $team1 $team0]} {
                    lset is_previous_home $team0 0
                    lset is_previous_home $team1 1
                } else {
                    error "unexpected"
                }
                lset first_round $iday $imatch $match
            }
            set rest [lcycle $rest]
        }

        set second_round [list]
        for {set iday 0} {$iday < $ndays} {incr iday} {
            for {set imatch 0} {$imatch < $nmatches} {incr imatch} {
                lset second_round $iday $imatch [lreverse [lindex $first_round $iday $imatch]]
            }
        }

        set r [list]
        for {set iround 0} {$iround < $nrounds} {incr iround} {
            if {$iround % 2 == 0} {
                set r [concat $r $first_round]
            } else {
                set r [concat $r $second_round]
            }
        }
        return $r
    }

# Generate countries
    for {set i 1} {$i <= $conf(n_countries)} {incr i} {
        set country_id [new_id]
        set country_name "country_$i"
        db eval {insert into country values ($country_id, $country_name)}
# Generate divisions
        for {set j 1} {$j <= $conf(n_divisions_per_country)} {incr j} {
            set division_id [new_id]
            set division_rank $j
            db eval {
                insert into division (id, rank, country_id)
                values
                ($division_id, $division_rank, $country_id)
            }
# Generate teams
            for {set k 1} {$k <= $conf(n_teams_per_division)} {incr k} {
                set team_id [new_id]
                set team_name "team_${i}_${division_rank}_$k"
                set team_division_id [new_id]
                db eval {
                    insert into team
                    (id, name)
                    values
                    ($team_id, $team_name)
                }
# Leave the season_id as NULL and it will be filled when the first season starts.
                db eval {
                    insert into teamdivision
                    (id, team_id, division_id, season_id)
                    values
                    ($team_division_id, $team_id, $division_id, NULL)
                }
# Generate players
                for {set iplayer 0} {$iplayer < $conf(n_players_per_team)} {incr iplayer} {
                    set player_id [new_id]
                    set player_name $player_id
# Have a base ability so that there is correlation between att and def
                    set player_ability [rand_logistic 0 1]
                    set player_ability_att [rand_logistic $player_ability 1]
                    set player_ability_def [rand_logistic $player_ability 1]
                    set player_velocity [expr {exp([rand_logistic 0 1])}]
                    db eval {
                        insert into player
                        (id, name, ability_att, ability_def, velocity)
                        values
                        ($player_id, $player_name,
                         $player_ability_att, $player_ability_def, $player_velocity)
                    }
                    set playerteam_id [new_id]
                    db eval {
                        insert into playerteam
                        (id, player_id, team_id, date_from, date_to)
                        values
                        ($playerteam_id, $player_id, $team_id, 0, NULL)
                    }
                }
            }
        }
    }

    set initialized_teamdivision_records 0
    set current_date $conf(date_start)
    set year 0
    set season_id ""
    while {$current_date < $conf(date_end)} {
        if {$current_date % $n_seconds_per_year == $conf(season_start_year_offset)} {
            # Schedule season
            puts "Scheduling season..."
            set previous_season_id $season_id
            set season_id [new_id]
            incr year
            db eval {
                insert into season
                (id, year, season_start)
                values
                ($season_id, $year, $current_date)
            }
            if {$initialized_teamdivision_records == 0} {
                db eval {
                    update teamdivision
                    set season_id = $season_id
                }
                incr initialized_teamdivision_records
            } else {
# Calculate standings and update teamdivision for promotion/relegation.
                db eval {
                    select d.id, d.rank
                    from division d
                    order by d.country_id, d.rank
                } division_row {
                    set standings [calc_division_standings $division_row(id) $previous_season_id]
# Promote top teams
                    set istanding -1
                    foreach standing_dict $standings {
                        incr istanding
                        set team_id [dict get $standing_dict team_id]
                        db eval {
                            insert into teamdivision
                            (team_id, division_id, season_id)
                            values
                            (
                                $team_id,
                                (
                                    select dnew.id
                                    from division dold
                                    join division dnew
                                        on dnew.country_id = dold.country_id
                                        and dnew.rank = (
                                            case
                                                when
                                                    $conf(promotion_relegation_enabled)
                                                    and $istanding <= 2
                                                then
                                                    case
                                                        when dold.rank = 1 then dold.rank
                                                        else dold.rank - 1
                                                    end
                                                when
                                                    $conf(promotion_relegation_enabled)
                                                    and $istanding
                                                        >= $conf(n_teams_per_division) - 3
                                                then
                                                    case
                                                        when dold.rank =
                                                            $conf(n_divisions_per_country)
                                                        then dold.rank
                                                        else dold.rank + 1
                                                    end
                                                else
                                                    dold.rank
                                           end
                                        )
                                    where dold.id = $division_row(id)
                                ),
                                $season_id
                            )
                        }
                    }
                }
            }
            db eval {
                select *
                from division
                order by country_id, rank
            } {
                set division_id $id
                set team_ids [list]
                db eval {
                    select team.id team_id
                    from team
                    join teamdivision td on td.team_id = team.id
                    where td.division_id = $division_id
                    and td.season_id = $season_id
                } {
                    lappend team_ids $team_id
                }
                set schedule [gen_round_robin [llength $team_ids] 2]
                set dayd $current_date
                foreach dayl $schedule {
                    foreach matchl $dayl {
                        set match_id [new_id]
                        set team1_id [lindex $team_ids [lindex $matchl 0]]
                        set team2_id [lindex $team_ids [lindex $matchl 1]]
                        db eval {
                            insert into match
                            (
                                id,
                                date,
                                division_id,
                                season_id,
                                team1_id,
                                team2_id
                            )
                            values
                            (
                                $match_id,
                                $dayd,
                                $division_id,
                                $season_id,
                                $team1_id,
                                $team2_id
                            )
                        }
                    }
                    set dayd [expr {$dayd + $n_seconds_per_week}]
                }
            }
        }
# Find matches scheduled for today.
        db eval {
            select *
            from match
            where date = $current_date
        } match_row {
# Find total ability and calculate scores and save them.
            set team_data [list]
            foreach team_id [list $match_row(team1_id) $match_row(team2_id)] {
                db eval {
                    select
                    sum(p.ability_att) total_ability_att,
                    sum(p.ability_def) total_ability_def,
                    sum(p.velocity) total_velocity
                    from team
                    join playerteam pt on pt.team_id = team.id
                    join player p on p.id = pt.player_id
                    where team.id = $team_id
                    and pt.date_from <= $current_date
                    and (pt.date_to is null or pt.date_to > $current_date)
                } row {
                    lappend team_data [array get row]
                }
            }
# Play match
            set att1 [dict get [lindex $team_data 0] total_ability_att]
            set att2 [dict get [lindex $team_data 1] total_ability_att]
            set def1 [dict get [lindex $team_data 0] total_ability_def]
            set def2 [dict get [lindex $team_data 1] total_ability_def]
            set vel1 [dict get [lindex $team_data 0] total_velocity]
            set vel2 [dict get [lindex $team_data 1] total_velocity]
            set p1 [expr {[logistic_cdf 0 1 [expr {$att1 - $def2}]] / 28.}]
            set p2 [expr {[logistic_cdf 0 1 [expr {$att2 - $def1}]] / 38.}]
            set n [expr {11.5/(1./$vel1 + 1./$vel2)}]
            set score1 [rand_binomial $p1 $n]
            set score2 [rand_binomial $p2 $n]
            db eval {
                update match
                set
                score_team1 = $score1,
                score_team2 = $score2
                where id = $match_row(id)
            }
        }
        incr current_date $n_seconds_per_day
    }
}

proc main2 {} {
    #sqlite3 db :memory:
    sqlite3 db "sportsim2.db"
    db transaction {
        main1
    }
}

main2
