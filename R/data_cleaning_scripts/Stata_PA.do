
// cd "/Users/williamlief/Documents/Research Projects/Pennsylvania"
cd "/Volumes/SSD/DataHouse/StateData/Pennsylvania/Payroll"

* list of excel files and sheet names.
* variables are not consistent throughout -biggest dispairty seems to be 2009-10 to 2010-11 when a bunch more data gets added in (including the charter school identifier). 
* why are there twice as many observations in 2008-2009?


import excel, clear firstrow allstring, ///
	using "2007-08 Professional Personnel Individual Staff Report.xlsx", sheet("_2007_2008_Active_Professional_")
	gen year2 = "2007_2008"
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2008-09 Professional Personnel Individual Staff Report.xlsx", sheet("0809 PA Active Professional Per") 
	gen year2 = "2008_2009"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2009-10 Professional Personnel Individual Staff Report.xlsx", sheet("Public_Release_File_v2") 
	gen year2 = "2009_2010"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2010-11 Staff Public Release File.xlsx", sheet("tblStaffPublicReleaseData") 
	gen year2 = "2010_2011"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2011-12 Staff Public Release File.xlsx", sheet("tblStaffPublicReleaseData")  
	gen year2 = "2011_2012"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2012-13 Staff Public Release File.xlsx", sheet("2012-13 Staff Public Release")
	gen year2 = "2012_2013"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2013-14 Staff Public Release File.xlsx", sheet("tblStaffPublicRelease2013_14") 
	gen year2 = "2013_2014"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2014-15 Staff Public Release File.xlsx", sheet("tblStaffPublicRelease2014_15") 
	gen year2 = "2014_2015"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2015-16 Professional Personnel Individual Staff Report.xlsx", sheet("tblStaffPublicRelease2015_16")
	gen year2 = "2015_2016"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2016-17 Professional Personnel Individual Staff Report_revised.xlsx", sheet("tblStaffPublicRelease2016_17")
	gen year2 = "2016_2017"
	append using work/master
save work/master, replace

import excel, clear firstrow allstring, ///
	using "2017-18 Professional Personnel Individual Staff Report.xlsx", sheet("StaffPublicRelease2017-18")
	gen year2 = "2017_2018"
	append using work/master
save work/master, replace
