
/*

Cleansing Data in SQL Queries

*/

--------------------------------------------------------------------

-- Populate Property Address data

select PropertyAddress 
from PortfolioProject.NashivilleHousing nh 
where PropertyAddress = '';

-- for now on, there is no null data. 
-- but if PropertyAddress column has more than one null, we could guess that ParcelID points to PropertyAddress
-- so, we could replace PropertyAddress null data by data that has same ParcelID ,but not null data in PropertyAddress
-- we'll gonna use join itself conditioning same ParcelID, but not the same UniqueID. also we'll gonna see where PropertyAddress is null. 
-- and then we'll gonna change that a.PropertyAddress which is null to b.PropertyAddress by ISNULL function. -> ISNULL(a.PropertyAddress,b.PropertyAddress)
-- so, Using UPDATE statement like this
select a.ParcelID ,a.PropertyAddress , b.ParcelID , b.PropertyAddress,  #case when a.PropertyAddress = '' then b.PropertyAddress end 
from PortfolioProject.NashivilleHousing a
JOIN PortfolioProject.NashivilleHousing b
	on a.ParcelID = b.ParcelID
	and a.UniqueID <> b.UniqueID
WHERE a.PropertyAddress = '';




Update PortfolioProject.NashivilleHousing a
JOIN PortfolioProject.NashivilleHousing b
	on a.ParcelID = b.ParcelID
	and a.UniqueID <> b.UniqueID
SET a.PropertyAddress = case when a.PropertyAddress = '' then b.PropertyAddress else a.PropertyAddress end
WHERE a.PropertyAddress = '';

--------------------------------------------------------------------

-- breaking out address into individual columns(Address, City, State)
select PropertyAddress 
from PortfolioProject.NashivilleHousing nh


select SUBSTRING_INDEX(PropertyAddress,',',1) as Address -- ',' 를 기준으로 앞 부분 추출
	,SUBSTRING_INDEX(PropertyAddress,',',-1) as 
from PortfolioProject.NashivilleHousing nh


-- adding address, city column in table
alter table PortfolioProject.NashivilleHousing 
add `PropertySplitAddress` varchar(255);

alter table PortfolioProject.NashivilleHousing 
add `PropertySplitCity` varchar(255);

-- update address, city column by filling data
update PortfolioProject.NashivilleHousing 
set PropertySplitAddress = SUBSTRING_INDEX(PropertyAddress,',',1),
	PropertySplitCity = SUBSTRING_INDEX(PropertyAddress,',',-1)
	
select *
from PortfolioProject.NashivilleHousing nh;

-----------------------------------------------------------------------

-- OwnerAddress : let's split to Address, City, State


select OwnerAddress 
from PortfolioProject.NashivilleHousing nh;
