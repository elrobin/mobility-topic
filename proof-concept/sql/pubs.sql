-- Pubs: clusters, collab, dummy origin, impact, year


-- get publications
drop table #pubs
select a.researcher_id, a.country_code_origin, b.pub_id, c.pub_year, c.n_cits, e.country_code, g.cluster_id1,
  case when f.collaboration_type_no=3 then 1 else 0 end as int_collab
into #pubs
from userdb_robinsonn..CS_proof_concept_researchers as a
join dimensions_2022jun..pub_author as b on a.researcher_id=b.researcher_id
join dimensions_2022jun..pub as c on  b.pub_id=c.pub_id
join dimensions_2022jun..pub_author_affiliation as d on b.pub_id=d.pub_id and b.author_seq=d.author_seq
join dimensions_2022jun..pub_affiliation_country as e on d.pub_id=e.pub_id and d.affiliation_seq=e.affiliation_seq
join dimensions_2022jun_indicators..pub as f on b.pub_id=f.pub_id
join dimensions_2022jun_classification..clustering as g on f.pub_id=g.pub_id
--2765437
-- Convert add dummy co_origin
drop table userdb_robinsonn..cs_proof_concept_pubs
select researcher_id, pub_id, pub_year, n_cits, cluster_id1, country_code, int_collab,
  case when country_code_origin=country_code then 0 else 1 end as foreign_country
into userdb_robinsonn..cs_proof_concept_pubs
from #pubs
--2765437
select * from userdb_robinsonn..cs_proof_concept_pubs