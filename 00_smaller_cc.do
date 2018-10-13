/* subset chronic conditions to 271892
Eric Chow, Oct 10, 2018
*/

use "~/DATA_splc/CCflag_small.dta"

merge m:1 patient_id using "~/DATA_splc/patient_ids.dta"
keep if _merge == 3
drop _merge indic

saveold "~/DATA_splc/CCflag_small_271892.dta", v(12) replace
