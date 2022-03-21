run "/Users/williamlief/Documents/Research Projects/Ohio/Code/paths.do"
 
cd $salraw


! ls *.xlsx >filelist.txt

file open myfile using "filelist.txt", read
file read myfile line
di "`line'"
import excel "`line'", clear firstrow allstring
gen filename = "`line'"
drop if missing(Year) // importing a bunch of blank lines
foreach var of varlist * {
	replace `var' = strtrim(`var')
}
save master, replace

file read myfile line
while r(eof)==0 { 
	di "`line'"
	import excel "`line'", clear firstrow allstring
	gen filename = "`line'"
	
	* file specific adjustments
	cap drop if filename == "2008_Teacher_Salary.xlsx" & missing(Year) // also importing blank lines
	*
	
	foreach var of varlist * { 
		replace `var' = strtrim(`var')
	}

	qui append using master
	qui save master, replace
	file read myfile line
}
file close myfile
! rm filelist.txt

* combine vars
gen name_last = LASTNAME + Last_Name
gen name_first = FIRSTNAME + First_Name + FirstName
drop LASTNAME Last_Name FIRSTNAME First_Name FirstName 

gen county = COUNTY + COUNTYNAME
gen city = CITYNAME
gen school = SCHOOLNAME + Building 
gen district = DISTRICTNAME + District
drop COUNTY COUNTYNAME SCHOOLNAME Building DISTRICTNAME District

gen days_worked = DAYSWORKED + DaysWorked + Days_Worked
gen hours_worked = HOURSWORKEDPERDAY
gen degree = EDUCATIONLEVEL
gen salary = PAYAMOUNT + Salary
gen position = JOBDESCRIPTION + PositionCode + Job_Description
drop  DAYSWORKED DaysWorked Days_Worked EDUCATIONLEVEL PAYAMOUNT Salary CITYNAME  JOBDESCRIPTION PositionCode Job_Description HOURSWORKEDPERDAY

* make year vars
drop Year
gen year = real(substr(filename,1,4)) 
replace year = year+1 if substr(filename,5,1)=="-" | substr(filename,6,1)=="-" // files for 2007 through 2011 are named with the second year only, files for 2012 and on are named 2012-2013

gen year2 = string(year-1) + "_" + string(year)

* make numeric variables
rename salary csal
gen salary = real(csal)

rename (days_worked hours_worked) (cd ch)
gen days_worked = real(cd)
gen hours_worked = real(ch)
drop cd ch csal

* Standardize case
foreach var in name_last name_first county city school district position {
	replace `var' = lower(`var')
}

* label vars
label var filename "name of excel source file"
label var position "position codes, probably vary across years"
label var year "spring year value (eg 2012-13 school year = 2013)"
label var year2 "full academic year"
label var hours_worked "Hours worked per day, capped at 16 in reporting"
label var days_worked "Days worked per year, has impossible values"

* save file
save "$clean/ohio_full.dta", replace
erase "master.dta"
