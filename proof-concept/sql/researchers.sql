/****** INDIVIDUAL LEVEL MOBILITY TABLE - MOBILITY AND TOPICS PROJECT  ******/
/****** Dataset: period 2008-2020
- OVERALL: researcher_id, first_year, last_year, co_origin, co_final, n_countries, p, pp_origin,
mult_affil, mult_affil_mob
- YEARLY: pub_year, p_year, diff_co_origin, n_for_group_year, n_for_division_year 
******/

-- Raw table
drop table #raw
select  a.researcher_id, b.pub_id, b.pub_year, b.n_cits, 
  case when c.collaboration_type_no=1 then  0 else 1 end as collab,
  case when c.collaboration_type_no=3 then 1 else 0 end as int_collab,
  f.country_code
into #raw
from dimensions_2022jun..pub_author as a
join dimensions_2022jun..pub as b on a.pub_id=b.pub_id
join dimensions_2022jun_indicators..pub as c on b.pub_id=c.pub_id
join dimensions_2022jun..pub_author_affiliation as d on a.pub_id=d.pub_id and a.author_seq=d.author_seq
join dimensions_2022jun..pub_affiliation_country as f on f.pub_id=d.pub_id and f.affiliation_seq=d.affiliation_seq
where a.researcher_id is not null 
  and b.pub_year>2007
--238339125 00:12:01

                            ---------------------------------------
                            --      OVERALL
                            ---------------------------------------

-- Identify first publication 
-- Only authors with first publication since 2008 onwards 
drop table #firstpub0
select a.researcher_id, min(c.pub_year) as first_year
into #firstpub0
from #raw as a
join dimensions_2022jun..pub_author as b on a.researcher_id=b.researcher_id
join dimensions_2022jun..pub as c on c.pub_id=b.pub_id
join dimensions_2022jun..pub_author_affiliation as d on b.pub_id=d.pub_id and b.author_seq=d.author_seq
join dimensions_2022jun..pub_affiliation_organization as e on d.pub_id=e.pub_id and d.affiliation_seq=e.affiliation_seq
join dimensions_2022jun..pub_affiliation_country as f on f.pub_id=d.pub_id and f.affiliation_seq=d.affiliation_seq
group by a.researcher_id
--13392514
drop table #firstpub
select a.researcher_id, b.grid_id as grid_id_origin, b.country_code as country_code_origin, a.first_year
into #firstpub
from #firstpub0 as a
join #raw as b on a.researcher_id=b.researcher_id and a.first_year=b.pub_year
where (first_year between 2008 and 2010) 
  and 
  (b.country_code='ES' or b.country_code='ZA' or b.country_code='AR' or b.country_code='NL')
drop table #firstpub0
--178525

------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
select * from #firstpub
-- Total pubs, orgs and countries, fields
drop table #totalcounts0
select researcher_id, count(distinct pub_id) as p, count(distinct grid_id) as n_grid, count(distinct country_code) as n_country,
  count(distinct for_division_id) as n_for_division, count(distinct for_group_id) as n_for_group
into #totalcounts0
from #raw
group by researcher_id
--12130696
drop table #totalcounts
select *
into #totalcounts
from #totalcounts0
where p > 4
drop table #totalcounts0
--4492476
-- Total citations
drop table #cits
select distinct b.researcher_id, a.pub_id, a.n_cits
into #cits
from #raw as a
join #totalcounts as b on a.researcher_id=b.researcher_id
--91953293
drop table #tcs
select researcher_id, sum(n_cits) as tcs, cast(sum(n_cits) as float)/count(pub_id) as mcs
into #tcs
from #cits
group by researcher_id
drop table #cits
--4492476
-- Total collab
drop table #collab
select b.researcher_id, count(distinct a.pub_id) as p_collab
into #collab
from #raw as a
join #totalcounts as b on a.researcher_id=b.researcher_id
where a.collab=1
group by b.researcher_id
--4242799
-- Total int_collab
drop table #intcollab
select b.researcher_id, count(distinct a.pub_id) as p_int_collab
into #intcollab
from #raw as a
join #totalcounts as b on a.researcher_id=b.researcher_id
where a.int_collab=1
group by b.researcher_id
--3222788
-- Overall indivs
drop table #overall_indivs
select a.researcher_id, b.first_year, b.country_code_origin, b.grid_id_origin, a.p, c.tcs, c.mcs, d.p_collab, e.p_int_collab,
  a.n_country, a.n_grid, a.n_for_group, a.n_for_division
into #overall_indivs
from #totalcounts as a
join #firstpub as b on a.researcher_id=b.researcher_id
left join #tcs as c on a.researcher_id=c.researcher_id
left join #collab as d on a.researcher_id=d.researcher_id
left join #intcollab as e on a.researcher_id=e.researcher_id
--11156887
drop table userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project
select distinct *
into userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project
from #overall_indivs
--3150598
drop table #overall_indivs
drop table #collab
drop table #firstpub
drop table #intcollab
drop table #tcs
drop table #totalcounts

                            ---------------------------------------
                            --      YEARLY
                            ---------------------------------------

--select 
----top 100 * 
--count(distinct researcher_id) 
--from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project

-- Total counts yearly
drop table #yearcounts
select a.researcher_id, b.pub_year as pub_date, count(distinct pub_id) as p_year,
  count(distinct b.grid_id) as n_grid_year, count(distinct b.country_code) as n_country_year, 
  count(distinct b.for_group_id) as n_for_group_year, count(distinct b.for_division_id) as n_for_division_year
into #yearcounts
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join #raw as b on a.researcher_id=b.researcher_id
where a.first_year between 2008 and 2010
group by a.researcher_id, b.pub_year
--4753979
--Country difference
drop table #co_diff
select a.researcher_id, a.first_year, b.pub_year, a.country_code_origin, b.country_code, a.grid_id_origin, b.grid_id--,
into #co_diff
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join #raw as b on a.researcher_id=b.researcher_id
where a.first_year between 2008 and 2010
--36713370
drop table #co_diff2
select researcher_id, first_year, pub_year, 
  age=pub_year-first_year,
  case when country_code_origin=country_code then 0 else 1 end as diff_co_origin,
  case when grid_id_origin=grid_id then 0 else 1 end as diff_grid_origin
into #co_diff2
from #co_diff
--36713370
-- collab
drop table #collab
select a.researcher_id, b.pub_year, count(distinct b.pub_id) as p_collab_year
into #collab
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join #raw as b on a.researcher_id=b.researcher_id
where b.collab=1 and (a.first_year between 2008 and 2010)
group by a.researcher_id, b.pub_year
--3310951
-- int collab
drop table #intcollab
select a.researcher_id, b.pub_year, count(distinct b.pub_id) as p_int_collab_year
into #intcollab
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join #raw as b on a.researcher_id=b.researcher_id
where b.int_collab=1 and (a.first_year between 2008 and 2010)
group by a.researcher_id, b.pub_year
--1739736
-- citas
drop table #cits
select distinct a.researcher_id, b.pub_year, b.pub_id, b.n_cits
into #cits
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join #raw as b on a.researcher_id=b.researcher_id
where a.first_year between 2008 and 2010
--11736749
drop table #tcs
select researcher_id, pub_year, sum(n_cits) as tcs_year, cast(sum(n_cits) as float)/count(pub_id) as mcs_year
into #tcs
from #cits
group by researcher_id, pub_year
drop table #cits
--4753979
-- Overall table
drop table #yearly_indivs
select a.researcher_id, a.pub_date, d.age, d.diff_grid_origin, d.diff_co_origin, a.p_year, e.tcs_year, e.mcs_year,
  b.p_collab_year, c.p_int_collab_year, a.n_grid_year, a.n_country_year, a.n_for_group_year, a.n_for_division_year
into #yearly_indivs
from #yearcounts as a
left join #collab as b on a.researcher_id=b.researcher_id and a.pub_date=b.pub_year
left join #intcollab as c on a.researcher_id=c.researcher_id and a.pub_date=c.pub_year
left join #co_diff2 as d on a.researcher_id=d.researcher_id and a.pub_date=d.pub_year
left join #tcs as e on a.researcher_id=e.researcher_id and a.pub_date=e.pub_year
--36713370

drop table userdb_robinsonn..indiv_dimensions_2021may_yearly_indivs_mob_project
select distinct *
into userdb_robinsonn..indiv_dimensions_2021may_yearly_indivs_mob_project
from #yearly_indivs
--5882869

-- Copy paste
select a.*, b.pub_date, b.age, b.diff_grid_origin, b.diff_co_origin, b.p_year, b.tcs_year, isnull(b.p_collab_year,0) as p_collab_year, 
  isnull(b.p_int_collab_year,0) as p_int_collab_year, b.n_grid_year, b.n_country_year, b.n_for_group_year, n_for_division_year
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join userdb_robinsonn..indiv_dimensions_2021may_yearly_indivs_mob_project as b on a.researcher_id=b.researcher_id

                            ---------------------------------------
                            --      TEXT DATA
                            ---------------------------------------

--drop table userdb_robinsonn..text_med_dimensions_2021may_indivs_mob_project
select distinct b.pub_id, c.title, c.abstract
--into userdb_robinsonn..text_med_dimensions_2021may_indivs_mob_project
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join dimensions_2021may..pub_author as b on a.researcher_id=b.researcher_id
join dimensions_2021may_text..text_data as c on b.pub_id=c.pub_id
join dimensions_2021may..pub_for_division as d on b.pub_id=d.pub_id
where (a.first_year between 2008 and 2010)
  and d.for_division_id=11 -- Medical and Health Sciences
  and a.n_country > 1 -- only mobile researchers
--1680001
--drop table userdb_robinsonn..text_socialsci_dimensions_2021may_indivs_mob_project
select distinct b.pub_id, c.title, c.abstract
--into userdb_robinsonn..text_socialsci_dimensions_2021may_indivs_mob_project
from userdb_robinsonn..indiv_dimensions_2021may_overall_indivs_mob_project as a
join dimensions_2021may..pub_author as b on a.researcher_id=b.researcher_id
join dimensions_2021may_text..text_data as c on b.pub_id=c.pub_id
join dimensions_2021may..pub_for_division as d on b.pub_id=d.pub_id
where (a.first_year between 2008 and 2010)
  and (d.for_division_id=14 -- Economics 
       or d.for_division_id=13 -- Education
	   or d.for_division_id=16 -- Studies in Human Society
	   )
  and a.n_country > 1 -- only mobile researchers
--246441
