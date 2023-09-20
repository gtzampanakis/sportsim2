#!/usr/bin/tclsh
package require sqlite3

set conf(n_countries) 4
set conf(n_divisions_per_country) 3
set conf(n_teams_per_division) 16

proc main {} {
    global conf

    set fp [open "main.sql" r]
    set db_schema_sql [read $fp]
    close $fp

    sqlite3 db :memory:

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

    puts [gen_round_robin 6 4]

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
                db eval {
                    insert into team (id, name, division_id) values
                    ($team_id, $team_name, $division_id)
                }
            }
        }
    }

    db close
}

main
