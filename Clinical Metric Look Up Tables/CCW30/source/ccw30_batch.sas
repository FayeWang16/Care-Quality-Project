****************************************************************************************************
*                                           PROGRAM OVERVIEW
****************************************************************************************************
* PROGRAM: ccw_batch.sas  
*
* PROGRAMMER:      Lan Luo
*
*------------------------------------------------------------------------------
*  CHANGE LOG: 
*
*   Version   	Date       	Comment (reference external documentation if available)
*   -------   	--------   	--------------------------------------------------------
*   1.0       	2025-06-23	Manually query ccw30 conditions
*							;
***********************************************************************;
*
* DESCRIPTION: 	Chronic Condition 30 in before index date (reference period either according to ccw or hard code to 1 for all).
				Created to account for MA data, which we can use the mbsf_cc for because that's constructed using FFS data only.
*
* LANGUAGE:        SAS Version 9.4
*

* Related files used for this macro:
*	- ccw30_codelist.sas  = settings of pre-defined code lists for each condition, including exclusion
*	- ccw30_meta.xslx     = meta file used to define each condition, mostly for reference period and qualification types
*	- ccw30_qualificationgroup = not used in running this macro, but specifies the crosswalk from qualification group number to actual conditions.

*  Program inputs:                                                                                   
*	-SAS data file: &SCDMLIB..diagnosis
					input dataset with &idvar. and indexdt
					ccw30_meta.sas7bdat and ccw30_codelist.sas, which should be stored in the same folder as this script ccw30_batch.sas;
* 
*  Program outputs: 
*	-SAS data file (.SAS7BDAT format) containing
*                   
* PARAMETERS:
*	-CONDITION_LIST 	= List of ccw30 condition acrynoms as defined in ccw30_meta, default is all (ami ad anemia asthma afib bph cancer_breast cancer_colorectal cancer_endometrial cancer_lung cancer_prostate cancer_urologic cataract kidney copd depression diabetes glaucoma heartfailure hipfracture hyperlipidemia hypertension hypothyroidism ischemicheart dementia osteoporosis parkinson pneumonia asthritis stroketia)
*	-INFILE     		= Name of the Input file containing &idvar. and &indexdt. 
*	-SCDMLIB			= Name of the library which has SCDM datasest as sas7bdat;
*	-INDEXDT  			= Variable name of index date to check whether the person have any disease before this day
*	-IDVAR				= Variable name which uniquely specify rows (benes)
*	-RESTRICTREF		= if 1 then restrict all reference years to 1, if 0 then use ccw30 specified reference periods for each condition as specified in ccw30_meta
*	-DEBUG				= (optional)if 1: read only 10000 observation and do not delete interim files
									if 0: read all and delete interim files
*	-OUTFINAL    		= Name of Output file assign, default is ccw30_flags_all
*
* SAMPLE USAGE:
* %ccw30_batch(
	condition_list=
ami
ad
anemia
asthma
afib
bph
cancer_breast
cancer_colorectal
cancer_endometrial
cancer_lung
cancer_prostate
cancer_urologic
cataract
kidney
copd
depression
diabetes
glaucoma
heartfailure
hipfracture
hyperlipidemia
hypertension
hypothyroidism
ischemicheart
dementia
osteoporosis
parkinson
pneumonia
arthritis
stroketia
,
    infile=cohort_test,
    scdmlib=ffs,
    indexdt=indexdt,
    idvar=patid,
    restrictref=1,
    debug=0,
    outfinal=ccw30_flags_all	
);

***************************************************************************************************;
*MANUALLY QUERY CHRONIC CONDITION 30;

%macro ccw30_manual(
    conditionname=,       /* e.g., ami, afib, etc. refer to the first column in ccw30_meta.xslx */
    infile=,              /* input dataset with patid + index date */
    dxtable=dxtable,	  /* scdm diagnosis table AFTER filtering idvar and indexdt/ref period */
    indexdt=,      /* name of index date variable */
    idvar=,          /* name of patient ID variable */
    restrictref= ,       /* if restrictref=1, then restrict reference period to 1 for all conditions  */
    debug=              /* if debug=1, limit obs and keep temp files */
);



/*--- Step 1: Set debug options ---*/
%if &debug. = 1 %then %do;
    options obs=10000;
%end;

/* %local dxlist qual_group ref_period logicdesc; */

/*--- Step 2: Pull qualification rule and reference period from metadata ---*/
proc sql noprint;
    select 
        qualification_group, 
        needs_exclusion,
        ref_period,
        qualification,
        condition_name_long
    into 
        :qual_group trimmed, 
        :needs_exclusion trimmed,
        :ref_period trimmed,
        :qualification trimmed,
        :condition_name_long trimmed
    from ccw30_meta
    where  strip(upcase(condition_name)) = "%upcase(&conditionname)";
/*     condition_name = "&conditionname"; */
quit;

%if &restrictref. = 1 %then %do;
	%let ref_period = 1;
%end;

 %put NOTE: condition: %superq(conditionname) (%superq(condition_name_long)), 
    reference period restricted: %superq(restrictref),
    reference period after restriction if applicable: %superq(ref_period) year, 
    qualification group: %superq(qual_group), qualification details: %superq(qualification),
    needs exclusion: %superq(needs_exclusion);


/* pull codelist defined in ccw30_codelist */
%let dxlist = &&ccw_&conditionname._dx;

%put NOTE: &conditionname. codelist: &dxlist.;

%if &needs_exclusion. = 1 %then %do;
%put NOTE: &conditionname. exclusion codelist: &&ccw_&conditionname._exclude_dx;
%end;

%let outfile = outfile_ccw_&conditionname.;

/*--- Step 3: Apply logic based on qualification group ---*/

**********qual 1************************************;
%if &qual_group. = 1 %then %do;
/* Qual 1: At least 1 inpatient claim with DX codes*/
	proc sql;
    create table _dx_&conditionname as
    select &idvar., dx, adate, enctype, &indexdt., encounterid
	from &dxtable. 
    where enctype in ("IP", "IS", "IN")
      and dx_codetype = '10'
      and dx in (&&ccw_&conditionname._dx)
      and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
	/* keep one row per id per qualification rule */
	proc sort data = _dx_&conditionname. out = _dx_&conditionname. nodupkey;
		by &idvar.;
	run;
	
	/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname.;
		merge _dx_&conditionname. (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;
	%end;
%end;

/* **********qual 2************************************; */
%else %if &qual_group. = 2 %then %do; 
/* Qual 2: At least 1 inpatient/SNF/HHA claim OR 2 HOP/carrier claims with DX codes */
	proc sql;
    	create table _dx_&conditionname as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid,
        /* Create qualification group flag */
        case 
            when enctype in ("IP", "IS", "IN", "SN", "HA") then 1
            when enctype in ("ED", "OF", "OS", "OH", "OC", "CA") then 2
            else . 
        end as encgrp
    	from &dxtable. 
    	where enctype in ("IP", "IS", "IN", "SN", "HA",
    						"ED", "OF", "OS", "OH", "OC", "CA")
     	and dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
/* note here by encgrp instead of enctype to avoid bene having only 1op and 1carrier on the same day 	 */
	proc sort data=_dx_&conditionname nodupkey;
		by patid encgrp adate;
	run;
	
/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from  &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname;
		merge _dx_&conditionname (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;

	%end;

	proc sql;
  	create table _dx_&conditionname. as
  	select patid, adate
  	from _dx_&conditionname.
  	group by patid
  	having sum(encgrp = 1) >= 1 or sum(encgrp = 2) >= 2;
	quit;
%end;

/* **********qual 3************************************; */
%else %if &qual_group. = 3 %then %do;
/* qual group 3: At least 1 inpatient/SNF claim OR 2 HOP/carrier claims with DX codes */
proc sql;
    	create table _dx_&conditionname as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid,
        /* Create qualification group flag */
        case 
            when enctype in ("IP", "IS", "IN", "SN") then 1
            when enctype in ("ED", "OF", "OS", "OH", "OC", "CA") then 2
            else . 
        end as encgrp
    	from &dxtable. 
    	where enctype in ("IP", "IS", "IN", "SN",
    						"ED", "OF", "OS", "OH", "OC", "CA")
     	and dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
/* note here by encgrp instead of enctype to avoid bene having only 1op and 1carrier on the same day 	 */
	proc sort data=_dx_&conditionname nodupkey;
		by patid encgrp adate;
	run;
	
/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname;
		merge _dx_&conditionname (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;
	%end;

	proc sql;
  	create table _dx_&conditionname. as
  	select patid, adate
  	from _dx_&conditionname.
  	group by patid
  	having sum(encgrp = 1) >= 1 or sum(encgrp = 2) >= 2;
	quit;
%end;

/* **********qual 4************************************; */
%else %if &qual_group. = 4 %then %do;
/* Qual 4: At least 1 HOP or carrier claim with DX codes*/
	proc sql;
    create table _dx_&conditionname as
    select &idvar., dx, adate, enctype, &indexdt., encounterid
	from &dxtable.
    where enctype in ("ED", "OF", "OS", "OH", "OC", "CA")
      and dx_codetype = '10'
      and dx in (&&ccw_&conditionname._dx)
      and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
	/* keep one row per id per qualification rule */
	proc sort data = _dx_&conditionname. nodupkey;
		by &idvar.;
	run;
	
	/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname.;
		merge _dx_&conditionname. (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;
	%end;	
%end;

/* **********qual 5************************************; */
%else %if &qual_group. = 5 %then %do;
/* Qual 5: At least 1 inpatient, SNF, HOP or carrier claim with DX codes*/
	proc sql;
    create table _dx_&conditionname as
    select &idvar., dx, adate, enctype, &indexdt., encounterid
	from &dxtable.
    where enctype in ("IP", "IS", "IN", "SN", 
    						"ED", "OF", "OS", "OH", "OC", "CA")
      and dx_codetype = '10'
      and dx in (&&ccw_&conditionname._dx)
      and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
	/* keep one row per id per qualification rule */
	proc sort data = _dx_&conditionname. nodupkey;
		by &idvar.;
	run;
	
	/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname.;
		merge _dx_&conditionname. (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;
	%end;	
%end;

/* **********qual 6************************************; */
%else %if &qual_group. = 6 %then %do;
/* Qual 6: At least 1 inpatient, HOP or carrier claim with DX codes*/
	proc sql;
    create table _dx_&conditionname as
    select &idvar., dx, adate, enctype, &indexdt., encounterid
	from &dxtable.
    where enctype in ("IP", "IS", "IN",  
    						"ED", "OF", "OS", "OH", "OC", "CA")
      and dx_codetype = '10'
      and dx in (&&ccw_&conditionname._dx)
      and adate between &indexdt.-365*&ref_period. and &indexdt.;
	quit;
	
	/* keep one row per id per qualification rule */
	proc sort data = _dx_&conditionname. nodupkey;
		by &idvar.;
	run;
	
	/* exclude encounters with exclusion codes	 */
	%if &needs_exclusion. = 1 %then %do;
		proc sql;
    	create table _exclude as
    	select &idvar., dx, adate, enctype, &indexdt., encounterid
    	from &dxtable. 
    	where dx_codetype = '10'
      	and dx in (&&ccw_&conditionname._exclude_dx)
      	and adate between &indexdt.-365*&ref_period. and &indexdt.;
		quit;
		
		proc sort data=_exclude nodupkey;
		by patid encounterid;
		run;
		
/* 		data _dx_&conditionname; */
/* 		merge _dx_&conditionname (in=a) */
/* 		_exclude(in=b); */
/* 		if a and not b; */
/* 		run; */
		proc sort data=_dx_&conditionname.;
		by patid encounterid;
		run;
				
		data _dx_&conditionname.;
		merge _dx_&conditionname. (in=a)
		_exclude(in=b);
		by encounterid;
		if a and not b;
		run;
	%end;	
%end;

/* outfile  */
/* data &outfile.; */
/* 	merge &infile. (in=a ) */
/* 	_dx_&conditionname. (in=b keep = &idvar. ); */
/* 	by &idvar.; */
/* 	if a; */
/* 	ccw_&conditionname._ind = b; */
/* run; */
data &outfile.;
    merge &infile. (in=a)
          _dx_&conditionname. (in=b keep = &idvar. adate);
    by &idvar.;
    if a;
    if b and adate <= &indexdt. and adate >= &indexdt.-365*&ref_period. then ccw_&conditionname._ind = 1;
    else ccw_&conditionname._ind = 0;
run;
	/* quick stats	 */
proc sql noprint;
    select 
        count(*) as total,
        sum(ccw_&conditionname._ind) as with_condition,
        calculated with_condition / calculated total * 100 as pct_with_condition
    into 
        :n_total, :n_with, :pct_with
    from &outfile.;
quit;


%put NOTE: &n_with out of &n_total patients (&pct_with%) had %superq(conditionname) (%superq(condition_name_long));

/*--- Step 4: Cleanup ---*/
%if &debug = 0 %then %do;
    proc datasets lib=work nolist;
        delete _dx_&conditionname.  _exclude;
    quit;
%end;

options obs=max;

%mend ccw30_manual;


%macro ccw30_batch(
	condition_list=ami
ad
anemia
asthma
afib
bph
cancer_breast
cancer_colorectal
cancer_endometrial
cancer_lung
cancer_prostate
cancer_urologic
cataract
kidney
copd
depression
diabetes
glaucoma
heartfailure
hipfracture
hyperlipidemia
hypertension
hypothyroidism
ischemicheart
dementia
osteoporosis
parkinson
pneumonia
arthritis
stroketia,
    infile=,
    scdmlib=ffs,
    indexdt=indexdt,
    idvar=patid,
    restrictref=1,
    debug=0,
    outfinal=ccw30_flags_all	
);


* read in needed files first;
%include "&prog./autocall/ccw30_codelist.sas";
data ccw30_meta;
set "&prog./autocall/ccw30_meta.sas7bdat";
run;


%if &debug. = 1 %then %do;
    options obs=10000;
%end;

/* pre-query subset of dx */
%if &restrictref.=1 %then %do;
	data dxtable;
	merge &scdmlib..diagnosis (in=a keep=&idvar. encounterid adate dx dx_codetype enctype)
          &infile. (in=b keep=&idvar. &indexdt.);
    by &idvar.;
    if a and b;
    if adate >= &indexdt. - 365 and adate <= &indexdt.;
	run;
%end;
%else %do;
	data dxtable;
	merge &scdmlib..diagnosis (in=a keep=&idvar. encounterid adate dx dx_codetype enctype)
          &infile. (in=b keep=&idvar. &indexdt.);
    by &idvar.;
    if a and b;
    if adate >= &indexdt. - 365*2 and adate <= &indexdt.;
	run;
%end;

	%local i nconds nextcond infile_loop prev_outfile;
    
    %let nconds = %sysfunc(countw(&condition_list.));
    %let infile_loop = &infile.; /* First iteration uses original infile */

    %do i = 1 %to &nconds;
        %let nextcond = %scan(&condition_list., &i);

        %ccw30_manual(
            conditionname=&nextcond,
            infile=&infile_loop,
            dxtable=dxtable,
            indexdt=&indexdt,
            idvar=&idvar,
            restrictref=&restrictref,
            debug=&debug
        );

        /* Update infile_loop to point to current output so next iteration uses it */
/*         %let infile_loop = outfile_ccw_&nextcond.; */
		 /* Delete previous dataset unless it's the first loop */
    %if &i > 1 %then %do;
        proc datasets lib=work nolist;
            delete &prev_outfile.;
        quit;
    %end;

    /* Update infile_loop and prev_outfile */
    %let prev_outfile = outfile_ccw_&nextcond.;
    %let infile_loop = &prev_outfile.;
     
    %end;

    /* Rename final dataset to desired output name */
    data &outfinal.;
        set &infile_loop.;
    run;
    
    proc datasets lib=work nolist;
        delete &infile_loop.;
    quit;
    
    title"prevalence";
/*  quick look at prevalence for conditions   */
proc means data=&outfinal. n mean maxdec=2;
    var ccw:;
run;
title;


%mend ccw30_batch;
