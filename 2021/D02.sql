-- Problem 1
SELECT
	SUM(f.sum) as horizontal,
    SUM(d.sum - u.sum) as vertical,
    (SUM(f.sum) * SUM(d.sum - u.sum)) as solution
FROM
	(SELECT SUM(num) FROM data WHERE movement = 'forward') as f,
    (SELECT SUM(num) FROM data WHERE movement = 'up') as u,
    (SELECT SUM(num) FROM data WHERE movement = 'down') as d;
    
-- Problem 2
WITH RECURSIVE foo (line, horizontal, vertical, aim) AS (
    (SELECT 1, 0, 0, 0)
    UNION
    (SELECT
   		(f.line + 1),
  		(CASE
     		WHEN d.movement = 'forward' THEN (f.horizontal + d.num)
     		ELSE f.horizontal
     	END),
  		(CASE
     		WHEN d.movement = 'forward' THEN (f.vertical + (d.num * f.aim))
     		ELSE f.vertical
     	END),
  		(CASE
     		WHEN d.movement = 'up' THEN (f.aim - d.num)
     		WHEN d.movement = 'down' THEN (f.aim + d.num)
     		ELSE f.aim
    	END)
    FROM data d
   	INNER JOIN foo f ON f.line = d.line
   )
)
SELECT horizontal, vertical, (horizontal * vertical) as solution
FROM foo
WHERE line = (SELECT MAX(line) FROM foo);
