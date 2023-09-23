select d.country_id, d.rank, avg(p.ability_att), avg(p.ability_def), avg(p.velocity)
from teamdivision td
join division d on d.id = td.division_id
join team t on t.id = td.team_id
join playerteam pt on t.id = pt.team_id
join player p on p.id = pt.player_id
group by d.country_id, d.rank
