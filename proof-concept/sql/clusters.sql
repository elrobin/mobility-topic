-- Get paper from 2008 to 2021
drop table #pubs
select a.cluster_id1, a.pub_id, b.pub_year, b.n_cits, d.country_code
into #pubs
from dimensions_2022jun_classification..clustering as a
join dimensions_2022jun..pub as b on a.pub_id=b.pub_id
join dimensions_2022jun..pub_affiliation as c on b.pub_id=c.pub_id
join dimensions_2022jun..pub_affiliation_country as d on c.pub_id=d.pub_id and c.affiliation_seq = d.affiliation_seq
where b.pub_year > 2007
--79670179
-- Count for each countries number of publications

drop table #coxp
select cluster_id1, pub_id, count(distinct country_code) as n_co
into #coxp
from #pubs
group by cluster_id1, pub_id
--32376329
drop table #collab
select cluster_id1,
  case when n_co > 1 then 1 else 0 end as int_collab
into #collab
from #coxp
--32376329
drop table #p_int
select cluster_id1, sum(int_collab) as p_int_collab
into #p_int
from #collab
group by cluster_id1
--865
drop table #cits
select cluster_id1, count(distinct country_code) as n_co, sum(n_cits) as t_cits, count(distinct pub_id) as p
into #cits
from #pubs
group by cluster_id1
--865
drop table userdb_robinsonn..CS_proof_concept_clusters
select a.cluster_id1, a.n_co, a.t_cits, a.p, b.p_int_collab
into userdb_robinsonn..CS_proof_concept_clusters
from #cits as a
join #p_int as b on a.cluster_id1=b.cluster_id1


select *
from userdb_robinsonn..CS_proof_concept_clusters
