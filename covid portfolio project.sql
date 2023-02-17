select *
from PortfolioProject.CovidDeaths cd
where continent != ''; -- 데이터 값 중에 continent 가 NULL로 되어있는 경우, 대륙에 대한 집계데이터이므로 빼야한다.


-- 1. total cases vs total deaths
select location , `date` , total_cases , total_deaths , population , round((total_deaths/total_cases)*100,2) as deaths_pct 
from PortfolioProject.CovidDeaths cd
WHERE location = 'India'
and continent != '';

-- 2. total cases vs population
select location , `date` , total_cases ,population , round((total_cases /population)*100,2) as covid_pct
from PortfolioProject.CovidDeaths cd
-- WHERE location = 'South Korea'
where continent != ''
order by `date`;

-- looking at countries with highest infection rate compared to population
select location 
	,population 
	,max(total_cases) as HighestInfectionCount
	,round(max(total_cases / population)*100,2) as HighestInfectedPct
from PortfolioProject.CovidDeaths cd
where continent != ''
group by location, population 
order by HighestInfectedPct desc;


-- showing countries with highest death count per population
select location 
	,population 
	,max(total_deaths) as HighestDeathCount
from PortfolioProject.CovidDeaths cd
where continent != ''
group by location, population
having location not in ('World','High income','Upper middle income','Lower middle income','Europe','Asia','North America','South America','European Union','Low income')
order by HighestDeathCount desc;

-- showing continents with the highest death count per population
select continent 
	,max(total_deaths) as HighestDeathCount
from PortfolioProject.CovidDeaths cd
where continent != ''
group by continent 
order by HighestDeathCount desc;

-- global numbers by date
select date
	,sum(new_cases) as total_cases
	,sum(new_deaths) as total_deaths
	,round((sum(new_deaths) / sum(new_cases))*100,2) as death_pct_date
from PortfolioProject.CovidDeaths cd 
where continent != ''
group by date;


-- join covidvaccine table and coviddeaths table
-- looking at total population vs vaccinations

-- cte
with PopvsVac (Continent, Location,`Date`, Population, New_Vaccnations, Cum_Vaccinated_Cnt)
as (
select cd.continent 
	,cd.location 
	,cd.`date` 
	,cd.population 
	,cv.new_vaccinations 
	,sum(cv.new_vaccinations) over(partition by cv.location order by cv.`date`) as cum_vaccinated_cnt
from PortfolioProject.CovidDeaths cd 
join PortfolioProject.CovidVaccinations cv 
	on cd.location  = cv.location 
	and cd.`date`  = cv.`date` 
where cd.continent != ''
)

select *
	,round((Cum_Vaccinated_Cnt / Population)*100,2)
from PopvsVac

-- temp table
-- drop table if exists PercentPopulationVaccinated : 만든 테이블 변경사항 생기면
create table PercentPopulationVaccinated -- 새 테이블 생성시에는 db 설정을 꼭 해줄것
(
Continent varchar(50),
Location varchar(50),
`Date` varchar(50),
Population bigint,
New_Vaccnations varchar(50),
Cum_Vaccinated_Cnt varchar(50)
)

insert into PercentPopulationVaccinated
select cd.continent 
	,cd.location 
	,cd.`date` 
	,cd.population 
	,cv.new_vaccinations 
	,sum(cv.new_vaccinations) over(partition by cv.location order by cv.`date`) as cum_vaccinated_cnt
from PortfolioProject.CovidDeaths cd 
join PortfolioProject.CovidVaccinations cv 
	on cd.location  = cv.location 
	and cd.`date`  = cv.`date` 
where cd.continent != '';

select *
	,round((Cum_Vaccinated_Cnt / Population)*100,2)
from PercentPopulationVaccinated;


-- creating view to store data for later visualizations

create view PercentPopulationVaccinated_view as -- view 는 기존테이블과 이름이 같으면 안된다.
select cd.continent 
	,cd.location 
	,cd.`date` 
	,cd.population 
	,cv.new_vaccinations 
	,sum(cv.new_vaccinations) over(partition by cv.location order by cv.`date`) as cum_vaccinated_cnt
from PortfolioProject.CovidDeaths cd 
join PortfolioProject.CovidVaccinations cv 
	on cd.location  = cv.location 
	and cd.`date`  = cv.`date` 
where cd.continent != '';

select *
from PercentPopulationVaccinated_view


