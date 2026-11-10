SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';
DROP TABLE IF EXISTS "Web analytics Table";

DROP TABLE IF EXISTS web_analytics;

CREATE TABLE web_analytics (
    record_id     INTEGER PRIMARY KEY,
    title_clean   TEXT,
    content_clean TEXT,
    numeric_views INTEGER,
    author        TEXT,
    pub_date      DATE
);

SELECT COUNT(*) FROM web_analytics;

--verification
SELECT COUNT(*) AS rows_loaded FROM web_analytics;               

SELECT record_id, numeric_views FROM web_analytics
WHERE record_id IN (3, 11);                                      

SELECT numeric_views, COUNT(*) AS n FROM web_analytics
GROUP BY 1 HAVING COUNT(*) > 1;                                  

SELECT COUNT(*) FILTER (WHERE author = 'unknown') AS missing_authors,
       COUNT(*) FILTER (WHERE pub_date IS NULL)   AS missing_dates
FROM web_analytics;                                              

SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY length(content_clean)) AS median_length
FROM web_analytics
WHERE content_clean <> 'no content available';

--query 1: summary length vs engagement
WITH m AS (
  SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY length(content_clean)) AS med
  FROM web_analytics
  WHERE content_clean <> 'no content available')
SELECT CASE WHEN length(w.content_clean) > m.med
            THEN 'Long summary' ELSE 'Short summary' END AS length_category,
       COUNT(*)                       AS article_count,
       ROUND(AVG(w.numeric_views), 2) AS avg_view_count
FROM web_analytics w CROSS JOIN m
WHERE w.content_clean <> 'no content available'
GROUP BY 1
ORDER BY 1;

--query 2: top 5 articles by views
SELECT record_id, title_clean, numeric_views,
       DENSE_RANK() OVER (ORDER BY numeric_views DESC) AS view_rank
FROM web_analytics
ORDER BY view_rank, record_id
LIMIT 5;