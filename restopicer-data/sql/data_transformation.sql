-- code for reference
-- mysqldump -uyuli -pyuli --add-locks --add-drop-table --extended-insert > yuli20141227.sql
-- mysql> show variables like ‘max_allowed_packet';
-- mysql> show variables like ‘net_buffer_length';
-- mysqldump -uroot -p*** 原数据库 -e –max_allowed_packet=4194304 –net_buffer_length=16384 > file.sql

-- mysqladmin create yuli
-- mysql yuli < yuli20141227.sql

use yuli;
source yuli20141225.sql

mysql -uyuli -p -e "create table yuli.paper_citation (select * from (select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.item_UT=yuli.paper.item_ut union select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.R9=yuli.paper.item_t9) as tmp)" &

select * from issue_title where full_source_title 
in ('MIS Quarterly','Information Systems Research','ACM Transactions on information systems',
'Management Science','Information & Management','Decision Support Systems',
'Journal of Management Information Systems','Journal of Electronic Commerce Research',
'Electronic Commerce Research','International Journal of Electronic Commerce')
limit 10;

select * from issue_title where full_source_title  = 'International Journal of Electronic Commerce'
limit 10;
--all need distinct!!!
--issue_title
create table yuli.issue_title (select distinct * from issue_title where full_source_title 
in ('MIS Quarterly','Information Systems Research','ACM Transactions on information systems',
'Management Science','Information & Management','Decision Support Systems',
'Journal of Management Information Systems','Journal of Electronic Commerce Research',
'Electronic Commerce Research','International Journal of Electronic Commerce'));
--issue_subject_category
--create table yuli.issue_subject_category (select * from issue_subject_category where issue_id in(select ui from yuli.issue_title));
create table yuli.issue_subject_category (select distinct issue_subject_category.* from issue_subject_category inner join yuli.issue_title on issue_subject_category.issue_id=yuli.issue_title.ui);
--issue
--create table yuli.issue (select * from issue where ui in (select ui from yuli.issue_title));
create table yuli.issue (select distinct issue.* from issue inner join yuli.issue_title on issue.ui=yuli.issue_title.ui);

--paper
--守护进程
--create table yuli.paper (select paper.* from paper inner join yuli.issue_title on left(paper.item_ut,10)=yuli.issue_title.ui);
--create table yuli.paper (select paper.* from paper where left(paper.item_ut,10) in (select ui from yuli.issue_title));
create table yuli.paper (select paper.* from paper where left(paper.item_ut,10) in (select ui from (select ui from yuli.issue_title) as tmp));
--paper_author_dais
create table yuli.paper_author_dais (select paper_author_dais.* from paper_author_dais inner join yuli.paper on paper_author_dais.ut=yuli.paper.item_ut);
--paper_author_info
create table yuli.paper_author_info (select paper_author_info.* from paper_author_info inner join yuli.paper on paper_author_info.ut=yuli.paper.item_ut);
--paper_author_keyword
create table yuli.paper_author_keyword (select paper_author_keyword.* from paper_author_keyword inner join yuli.paper on paper_author_keyword.ITEM_UT=yuli.paper.item_ut);
--paper_citation
--守护进程
create table yuli.paper_citation (select * from (select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.item_UT=yuli.paper.item_ut union select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.R9=yuli.paper.item_t9) as tmp);
create table yuli.paper_citationdup (select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.item_UT=yuli.paper.item_ut);
insert into yuli.paper_citationdup (select paper_citation.* from paper_citation inner join yuli.paper on paper_citation.R9=yuli.paper.item_t9);
create table yuli.paper_citation (SELECT DISTINCT * FROM yuli.paper_citationdup);
--paper_issue
create table yuli.paper_issue (select paper_issue.* from paper_issue inner join yuli.issue_title on paper_issue.item_ui=yuli.issue_title.ui);
--paper_keyword_plus
create table yuli.paper_keyword_plus (select paper_keyword_plus.* from paper_keyword_plus inner join yuli.paper on paper_keyword_plus.ITEM_UT=yuli.paper.item_ut);
--paper_text
create table yuli.paper_text (select paper_text.* from paper_text inner join yuli.paper on paper_text.item_ut=yuli.paper.item_ut);

--magazine
create table yuli.magazine (select distinct full_source_title from issue_title);
--keyword
mysql -uyuli -p -e "create table yuli.keywordtmp (select AUTHOR_KEYWORD as keyword from paper_author_keyword);" &
create table yuli.keywordtmp (select AUTHOR_KEYWORD as keyword from paper_author_keyword);
insert into yuli.keywordtmp (select KEYWORD_PLUS as keyword from paper_keyword_plus);
create table yuli.keyword (SELECT DISTINCT * FROM yuli.keywordtmp);
--plus keyword
mysql -uyuli -p -e "create table yuli.plus_keyword (select distinct KEYWORD_PLUS as keyword from paper_keyword_plus);" &
--author keyword
mysql -uyuli -p -e "create table yuli.author_keyword (select distinct AUTHOR_KEYWORD as keyword from paper_author_keyword);" &

--count group by
select tb1.full_source_title,count(*) from issue_title as tb1 inner join paper_issue as tb2 on tb1.ui=tb2.item_ui Group BY tb1.full_source_title

--paperid|issueid|year|magazine|category
select base.*,issue.publication_year,magazine.full_source_title,sc.subject_category from paper_issue as base inner join issue on base.item_ui = issue.ui inner join issue_title as magazine on base.item_ui = magazine.ui inner join issue_subject_category as sc on base.item_ui = sc.issue_id
create table yuli.paper_label(select base.*,issue.publication_year,magazine.full_source_title,sc.subject_category from paper_issue as base inner join issue on base.item_ui = issue.ui inner join issue_title as magazine on base.item_ui = magazine.ui inner join issue_subject_category as sc on base.item_ui = sc.issue_id)
--2015-07-15
create table restopicer_resource_info.issue(select distinct base.item_ui,magazine.full_source_title,issue.volume,issue.issue,issue.publication_type,issue.publication_year,issue.publication_date from paper_issue as base inner join issue on base.item_ui = issue.ui inner join issue_title as magazine on base.item_ui = magazine.ui)
create table restopicer_resource_info.paper(select distinct paper.item_ut,p.article_title,p.document_type,p.meeting_abstract_number,p.abstract,paper.beginning_page,paper.ending_page,paper.page_count,paper.item_t9,paper.cited_ref_count,paper.paper_id from paper inner join paper_text as p on paper.item_ut = p.item_ut)
create table paper_with_issue(select distinct paper.*,issue.* from issue_paper as base inner join paper on base.item_ut = paper.item_ut inner join issue on base.item_ui = issue.item_ui)
create table paper_issue_subject_category(select distinct base.item_ut,base.item_ui,issue.full_source_title,issue.publication_year,sc.subject_category,sc.sc_code,sc.product_code from issue_paper as base inner join issue on base.item_ui = issue.item_ui inner join yuli.issue_subject_category as sc on base.item_ui = sc.issue_id)
create table issue_subject_category(SELECT distinct `item_ui`, `full_source_title`, `publication_year`, `subject_category`, `sc_code`, `product_code` FROM `paper_issue_subject_category`)
create table paper_keyword_author(SELECT distinct paper.item_ut,paper.article_title,paper.abstract,r.AUTHOR_KEYWORD as author_keyword from paper inner join yuli.paper_author_keyword as r on paper.item_ut = r.ITEM_UT)
create table paper_keyword_plus(SELECT distinct paper.item_ut,paper.article_title,paper.abstract,r.KEYWORD_PLUS as plus_keyword from paper inner join yuli.paper_keyword_plus as r on paper.item_ut = r.ITEM_UT)
create table paper_keyword(select pkall.* from ((select paper_keyword_author.* from paper_keyword_author) UNION ALL (select paper_keyword_plus.* from paper_keyword_plus)) as pkall)
create table author_with_paper_and_issue(SELECT distinct d.DAIS ,i.item_ut,i.article_title,d.author_rank,i.abstract,i.document_type,i.cited_ref_count,i.item_ui,i.full_source_title,i.publication_type,i.publication_year,i.publication_date FROM yuli.paper_author_dais as d inner join paper_with_issue as i on d.ut=i.item_ut)
create table author(SELECT distinct d.DAIS, i.author_name, i.lastname,i.firstname,i.email FROM yuli.paper_author_dais as d inner join yuli.paper_author_info as i on d.ut=i.ut and d.author_rank=i.author_rank)
create table paper_author_info(SELECT distinct p.item_ut,p.article_title,d.author_rank,d.DAIS, i.author_name, i.lastname,i.firstname,i.email FROM yuli.paper_author_dais as d inner join yuli.paper_author_info as i on d.ut=i.ut and d.author_rank=i.author_rank inner join paper_with_issue as p on d.ut=p.item_ut)
create table paper_reference(select distinct p.item_ut,p.item_t9,p.article_title,p.item_ui,p.full_source_title,p.publication_year,p.cited_count,r.item_ut as ref_item_ut,c.R9 as item_r9,r.article_title as ref_title,c.cited_author as ref_author,c.cited_ref_year as ref_year,c.cited_work as ref_work,r.full_source_title as ref_work_full,c.cited_volume as ref_volume,c.cited_page as ref_issue,r.cited_count as ref_cited_count from yuli.paper_citation as c left join paper_with_issue as r on c.R9=r.item_t9 left join paper_with_issue as p on c.item_UT=p.item_ut)