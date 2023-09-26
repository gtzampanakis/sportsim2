select d.country_id, d.rank, avg(pa.att), avg(pa.def), avg(pa.vel)
from teamdivision td
join division d on d.id = td.division_id
join team t on t.id = td.team_id
join playerteam pt on t.id = pt.team_id
join player p on p.id = pt.player_id
join playerattr pa on pa.player_id = p.id
where pa.date = 0
group by d.country_id, d.rank
