# Fung_Heald_DMS
This is a repo for the modified CESM codes with a largely updated DMS oxidation scheme. For details, please refer to this [ACP paper](https://doi.org/10.5194/acp-2021-782).

## Background
In this repo, one would find the codes for rerunning the CESM simulation cases reported in __Table 1__ in the paper.

The codes are developed on the __NCAR's Cheyenne__ supercomputer system, based on the codebase located at `/glade/work/fvitt/cesm/cesm2.1.0_mosaic07/`.

## Usage
To rerun the simulation cases, please follow these steps:

1. create a case using the standard procedures of CESM from the codebase above.
2. copy all files to the directory of the created case
3. modify `env_run.xml` in the case directory accordingly:
    1. `CLM_BLDNML_OPTS` = `-bgc sp -megan -nofire`
    2. `CAM_NML_USE_CASE` = `2000_trop_strat_vbs_cam6`
    3. `CLM_NML_USE_CASE` = `2000_control`
    4. `CCSM_CO2_PPMV` = `367.0`
    5. `CLM_CO2_TYPE` = `constant`
4. modify `env_build.xml` in the case directory accordingly:
    1.  `CAM_CONFIG_OPTS` = `-phys cam6 -chem trop_strat_mam4_vbs -age_of_air_trcs -mosaic -usr_mech_infile {case_dir/SourceMod/chem_mech.in}`

## Special Inputs
To replicate the simulation cases, one would also require the below input files for:
1. [Sea surface DMS concentration](https://mycuhk-my.sharepoint.com/:u:/g/personal/1155013803_link_cuhk_edu_hk/ER84QGitO5ZNu5MZ5XZHcJwB5We9h4F2FLkbMJRzQ1UZdA?e=asmQTo) for sea-to-air DMS flux
2. [Volcanic SO2 emission inventory](https://mycuhk-my.sharepoint.com/:u:/g/personal/1155013803_link_cuhk_edu_hk/ERuYabFZKWxBrah0bjHafHgB5TaJ67XWsw38veSnsKTF8A?e=H2AeEL) for PI cases

---
For questions, please contact Ka Ming FUNG at kamingfung@link.cuhk.edu.hk.
