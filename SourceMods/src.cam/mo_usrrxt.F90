module mo_usrrxt

  use shr_kind_mod,     only : r8 => shr_kind_r8
  use cam_logfile,      only : iulog
  use ppgrid,           only : pver, pcols
  use seq_drydep_mod,   only : n_species_table, species_name_table, dheff  ! WSY for Phase Transfer; need access to the Henry's law constant

  implicit none

  private
  public :: usrrxt, usrrxt_inti, usrrxt_hrates

  save

  integer :: usr_O_O2_ndx
  integer :: usr_HO2_HO2_ndx
  integer :: usr_N2O5_M_ndx
  integer :: usr_HNO3_OH_ndx
  integer :: usr_HO2NO2_M_ndx
  integer :: usr_N2O5_aer_ndx
  integer :: usr_NO3_aer_ndx
  integer :: usr_NO2_aer_ndx
  integer :: usr_CO_OH_a_ndx
  integer :: usr_CO_OH_b_ndx
  integer :: usr_PAN_M_ndx
  integer :: usr_CH3COCH3_OH_ndx
  integer :: usr_MCO3_NO2_ndx
  integer :: usr_MPAN_M_ndx
  integer :: usr_XOOH_OH_ndx
  integer :: usr_SO2_OH_ndx
  integer :: usr_DMS_OHb_ndx    ! fkm for DMS intermediates
  integer :: usr_is_shift_1_ndx   ! fkm for iso pathway
  integer :: usr_is_shift_2_ndx   ! fkm for iso pathway
  integer :: usr_is_HPMTF_cloud_ndx ! fkm for HPMTF uptake
  integer :: usr_DMS_OH_ndx
  integer :: usr_HO2_aer_ndx
  integer :: usr_GLYOXAL_aer_ndx

  integer :: tag_NO2_NO3_ndx
  integer :: tag_NO2_OH_ndx
  integer :: tag_NO2_HO2_ndx
  integer :: tag_C2H4_OH_ndx
  integer :: tag_C3H6_OH_ndx
  integer :: tag_CH3CO3_NO2_ndx

!lke-TS1
  integer :: usr_PBZNIT_M_ndx
  integer :: tag_ACBZO2_NO2_ndx
  integer :: usr_ISOPNITA_aer_ndx
  integer :: usr_ISOPNITB_aer_ndx
  integer :: usr_ONITR_aer_ndx
  integer :: usr_HONITR_aer_ndx
  integer :: usr_TERPNIT_aer_ndx
  integer :: usr_NTERPOOH_aer_ndx
  integer :: usr_NC4CHO_aer_ndx
  integer :: usr_NC4CH2OH_aer_ndx
!
  integer :: usr_OA_O2_NDX
  integer :: usr_XNO2NO3_M_ndx
  integer :: usr_NO2XNO3_M_ndx
  integer :: usr_XHNO3_OH_ndx
  integer :: usr_XHO2NO2_M_ndx
  integer :: usr_XNO2NO3_aer_ndx
  integer :: usr_NO2XNO3_aer_ndx
  integer :: usr_XNO3_aer_ndx
  integer :: usr_XNO2_aer_ndx
  integer :: usr_XPAN_M_ndx
  integer :: usr_XMPAN_M_ndx
  integer :: usr_MCO3_XNO2_ndx

  integer :: usr_C2O3_NO2_ndx
  integer :: usr_C2H4_OH_ndx
  integer :: usr_XO2N_HO2_ndx
  integer :: usr_C2O3_XNO2_ndx

  integer :: tag_XO2N_NO_ndx
  integer :: tag_XO2_HO2_ndx
  integer :: tag_XO2_NO_ndx

  integer :: usr_O_O_ndx
  integer :: usr_CL2O2_M_ndx
  integer :: usr_SO3_H2O_ndx
  integer :: tag_CLO_CLO_M_ndx

  integer :: ion1_ndx, ion2_ndx, ion3_ndx, ion11_ndx
  integer :: elec1_ndx, elec2_ndx, elec3_ndx
  integer :: elec4_ndx, elec5_ndx, elec6_ndx
  integer :: het1_ndx

  integer, parameter :: nean = 3
  integer :: ean_ndx(nean)
  integer, parameter :: nrpe = 5
  integer :: rpe_ndx(nrpe)
  integer, parameter :: npir = 16
  integer :: pir_ndx(npir)
  integer, parameter :: nedn = 2
  integer :: edn_ndx(nedn)
  integer, parameter :: nnir = 13
  integer :: nir_ndx(nnir)
  integer, parameter :: niira = 112
  integer :: iira_ndx(niira)
  integer, parameter :: niirb = 14
  integer :: iirb_ndx(niirb)

  integer :: usr_clm_h2o_m_ndx, usr_clm_hcl_m_ndx
  integer :: usr_oh_co_ndx, het_no2_h2o_ndx, usr_oh_dms_ndx, aq_so2_h2o2_ndx, aq_so2_o3_ndx

  integer :: h2o_ndx
  
  ! ---------------------------------------------------------------
  ! WSY for Phase Transfer: phase-transfer and aqueous-phase chemistry on wet aerosols
  ! ---------------------------------------------------------------
  integer :: g2a_SO2_a1_ndx, g2a_SO2_a2_ndx, g2a_SO2_a3_ndx, a2g_SO2_a1_ndx, a2g_SO2_a2_ndx, a2g_SO2_a3_ndx
  integer :: g2a_H2O2_a1_ndx, g2a_H2O2_a2_ndx, g2a_H2O2_a3_ndx, a2g_H2O2_a1_ndx, a2g_H2O2_a2_ndx, a2g_H2O2_a3_ndx
  integer :: alwc_SIV_H2O2_a1_ndx, alwc_SIV_H2O2_a2_ndx, alwc_SIV_H2O2_a3_ndx
  
  integer :: g2a_DMS_a1_ndx, g2a_DMS_a2_ndx, g2a_DMS_a3_ndx, a2g_DMS_a1_ndx, a2g_DMS_a2_ndx, a2g_DMS_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_DMSO_a1_ndx, g2a_DMSO_a2_ndx, g2a_DMSO_a3_ndx, a2g_DMSO_a1_ndx, a2g_DMSO_a2_ndx, a2g_DMSO_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_MSIA_a1_ndx, g2a_MSIA_a2_ndx, g2a_MSIA_a3_ndx, a2g_MSIA_a1_ndx, a2g_MSIA_a2_ndx, a2g_MSIA_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_MSA_a1_ndx, g2a_MSA_a2_ndx, g2a_MSA_a3_ndx, a2g_MSA_a1_ndx, a2g_MSA_a2_ndx, a2g_MSA_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_O3_a1_ndx, g2a_O3_a2_ndx, g2a_O3_a3_ndx, a2g_O3_a1_ndx, a2g_O3_a2_ndx, a2g_O3_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_OH_a1_ndx, g2a_OH_a2_ndx, g2a_OH_a3_ndx, a2g_OH_a1_ndx, a2g_OH_a2_ndx, a2g_OH_a3_ndx   ! fkm for Phase Transfer
  integer :: g2a_HOBR_a1_ndx, g2a_HOBR_a2_ndx, g2a_HOBR_a3_ndx, a2g_HOBR_a1_ndx, a2g_HOBR_a2_ndx, a2g_HOBR_a3_ndx   ! fkm for Phase Transfer
  integer :: DMS_O3_a1_ndx, DMS_O3_a2_ndx, DMS_O3_a3_ndx        ! fkm for Phase Transfer
  integer :: DMSO_OH_a1_ndx, DMSO_OH_a2_ndx, DMSO_OH_a3_ndx     ! fkm for Phase Transfer
  integer :: MSIA_OH_a1_ndx, MSIA_OH_a2_ndx, MSIA_OH_a3_ndx     ! fkm for Phase Transfer
  integer :: MSIA_O3_a1_ndx, MSIA_O3_a2_ndx, MSIA_O3_a3_ndx     ! fkm for Phase Transfer
  integer :: MSA_OH_a1_ndx, MSA_OH_a2_ndx, MSA_OH_a3_ndx        ! fkm for Phase Transfer
  integer :: SIV_HOBR_a1_ndx, SIV_HOBR_a2_ndx, SIV_HOBR_a3_ndx  ! fkm for Phase Transfer
  integer :: SIV_O3_a1_ndx, SIV_O3_a2_ndx, SIV_O3_a3_ndx        ! fkm for Phase Transfer
  
  integer :: g2c_DMS_c_ndx, c2g_DMS_c_ndx  ! fkm for Cloud reactions
  integer :: g2c_DMSO_c_ndx, c2g_DMSO_c_ndx  ! fkm for Cloud reactions
  integer :: g2c_MSIA_c_ndx, c2g_MSIA_c_ndx  ! fkm for Cloud reactions
  integer :: g2c_MSA_c_ndx, c2g_MSA_c_ndx  ! fkm for Cloud reactions
  integer :: g2c_OH_c_ndx, c2g_OH_c_ndx  ! fkm for Cloud reactions
  integer :: g2c_O3_c_ndx, c2g_O3_c_ndx  ! fkm for Cloud reactions
  
  integer :: DMS_O3_c_ndx ! fkm for Cloud reactions
  integer :: DMSO_OH_c_ndx ! fkm for Cloud reactions
  integer :: MSIA_OH_c_ndx ! fkm for Cloud reactions
  integer :: MSIA_O3_c_ndx ! fkm for Cloud reactions
  integer :: MSA_OH_c_ndx ! fkm for Cloud reactions
  
  real(r8), parameter :: small_value = 1.e-12_r8      ! fkm for Phase Transfer
  
  integer :: qaerwat_idx         = 0 ! fkm for MOSAIC aLWC
  
  integer :: pH_idx         = 0      ! fkm for MOSAIC pH
  
  integer  :: drop_radius_idx = 0    ! added by fkm for cloud reactions
!
! jfl
!
  integer, parameter :: num_strat_tau = 22
  integer :: usr_strat_tau_ndx(num_strat_tau)
!
!lke++
  integer :: usr_COhc_OH_ndx
  integer :: usr_COme_OH_ndx
  integer :: usr_CO01_OH_ndx
  integer :: usr_CO02_OH_ndx
  integer :: usr_CO03_OH_ndx
  integer :: usr_CO04_OH_ndx
  integer :: usr_CO05_OH_ndx
  integer :: usr_CO06_OH_ndx
  integer :: usr_CO07_OH_ndx
  integer :: usr_CO08_OH_ndx
  integer :: usr_CO09_OH_ndx
  integer :: usr_CO10_OH_ndx
  integer :: usr_CO11_OH_ndx
  integer :: usr_CO12_OH_ndx
  integer :: usr_CO13_OH_ndx
  integer :: usr_CO14_OH_ndx
  integer :: usr_CO15_OH_ndx
  integer :: usr_CO16_OH_ndx
  integer :: usr_CO17_OH_ndx
  integer :: usr_CO18_OH_ndx
  integer :: usr_CO19_OH_ndx
  integer :: usr_CO20_OH_ndx
  integer :: usr_CO21_OH_ndx
  integer :: usr_CO22_OH_ndx
  integer :: usr_CO23_OH_ndx
  integer :: usr_CO24_OH_ndx
  integer :: usr_CO25_OH_ndx
  integer :: usr_CO26_OH_ndx
  integer :: usr_CO27_OH_ndx
  integer :: usr_CO28_OH_ndx
  integer :: usr_CO29_OH_ndx
  integer :: usr_CO30_OH_ndx
  integer :: usr_CO31_OH_ndx
  integer :: usr_CO32_OH_ndx
  integer :: usr_CO33_OH_ndx
  integer :: usr_CO34_OH_ndx
  integer :: usr_CO35_OH_ndx
  integer :: usr_CO36_OH_ndx
  integer :: usr_CO37_OH_ndx
  integer :: usr_CO38_OH_ndx
  integer :: usr_CO39_OH_ndx
  integer :: usr_CO40_OH_ndx
  integer :: usr_CO41_OH_ndx
  integer :: usr_CO42_OH_ndx
!lke--

  real(r8), parameter :: t0     = 300._r8                ! K
  real(r8), parameter :: trlim2 = 17._r8/3._r8           ! K
  real(r8), parameter :: trlim3 = 15._r8/3._r8           ! K

  logical :: has_ion_rxts, has_d_chem

contains

  subroutine usrrxt_inti
    !-----------------------------------------------------------------
    !        ... intialize the user reaction constants module
    !-----------------------------------------------------------------

    use mo_chem_utls,   only : get_rxt_ndx, get_spc_ndx
    use spmd_utils,     only : masterproc

    implicit none

    character(len=4) :: xchar
    character(len=32) :: rxtname
    integer :: i

!
! full tropospheric chemistry
!
    usr_O_O2_ndx         = get_rxt_ndx( 'usr_O_O2' )
    usr_HO2_HO2_ndx      = get_rxt_ndx( 'usr_HO2_HO2' )
    usr_N2O5_M_ndx       = get_rxt_ndx( 'usr_N2O5_M' )
    usr_HNO3_OH_ndx      = get_rxt_ndx( 'usr_HNO3_OH' )
    usr_HO2NO2_M_ndx     = get_rxt_ndx( 'usr_HO2NO2_M' )
    usr_N2O5_aer_ndx     = get_rxt_ndx( 'usr_N2O5_aer' )
    usr_NO3_aer_ndx      = get_rxt_ndx( 'usr_NO3_aer' )
    usr_NO2_aer_ndx      = get_rxt_ndx( 'usr_NO2_aer' )
    usr_CO_OH_a_ndx      = get_rxt_ndx( 'usr_CO_OH_a' )
    usr_CO_OH_b_ndx      = get_rxt_ndx( 'usr_CO_OH_b' )
    usr_PAN_M_ndx        = get_rxt_ndx( 'usr_PAN_M' )
    usr_CH3COCH3_OH_ndx  = get_rxt_ndx( 'usr_CH3COCH3_OH' )
    usr_MCO3_NO2_ndx     = get_rxt_ndx( 'usr_MCO3_NO2' )
    usr_MPAN_M_ndx       = get_rxt_ndx( 'usr_MPAN_M' )
    usr_XOOH_OH_ndx      = get_rxt_ndx( 'usr_XOOH_OH' )
    usr_SO2_OH_ndx       = get_rxt_ndx( 'usr_SO2_OH' )
    usr_DMS_OHb_ndx      = get_rxt_ndx( 'fkm_DMS_OHb')    ! fkm for DMS intermediates
    usr_DMS_OH_ndx       = get_rxt_ndx( 'usr_DMS_OH' )  ! commented fkm for DMS intermediates
    usr_is_shift_1_ndx   = get_rxt_ndx( 'is_shift_1')     ! fkm for iso pathway
    usr_is_shift_2_ndx   = get_rxt_ndx( 'is_shift_2')     ! fkm for iso pathway
    usr_is_HPMTF_cloud_ndx = get_rxt_ndx( 'is_HPMTF_cloud')     ! fkm for HPMTF uptake
    usr_HO2_aer_ndx      = get_rxt_ndx( 'usr_HO2_aer' )
    usr_GLYOXAL_aer_ndx  = get_rxt_ndx( 'usr_GLYOXAL_aer' )
 !
    tag_NO2_NO3_ndx      = get_rxt_ndx( 'tag_NO2_NO3' )
    tag_NO2_OH_ndx       = get_rxt_ndx( 'tag_NO2_OH' )
    tag_NO2_HO2_ndx      = get_rxt_ndx( 'tag_NO2_HO2' )
    tag_C2H4_OH_ndx      = get_rxt_ndx( 'tag_C2H4_OH' )
    tag_C3H6_OH_ndx      = get_rxt_ndx( 'tag_C3H6_OH' )
    tag_CH3CO3_NO2_ndx   = get_rxt_ndx( 'tag_CH3CO3_NO2' )
!lke-TS1
    usr_PBZNIT_M_ndx     = get_rxt_ndx( 'usr_PBZNIT_M' )
    tag_ACBZO2_NO2_ndx   = get_rxt_ndx( 'tag_ACBZO2_NO2' )
    usr_ISOPNITA_aer_ndx = get_rxt_ndx( 'usr_ISOPNITA_aer' )
    usr_ISOPNITB_aer_ndx = get_rxt_ndx( 'usr_ISOPNITB_aer' )
    usr_ONITR_aer_ndx    = get_rxt_ndx( 'usr_ONITR_aer' )
    usr_HONITR_aer_ndx   = get_rxt_ndx( 'usr_HONITR_aer' )
    usr_TERPNIT_aer_ndx  = get_rxt_ndx( 'usr_TERPNIT_aer' )
    usr_NTERPOOH_aer_ndx = get_rxt_ndx( 'usr_NTERPOOH_aer' )
    usr_NC4CHO_aer_ndx   = get_rxt_ndx( 'usr_NC4CHO_aer' )
    usr_NC4CH2OH_aer_ndx = get_rxt_ndx( 'usr_NC4CH2OH_aer' )
 !
 ! additional reactions for O3A/XNO
 !
    usr_OA_O2_ndx        = get_rxt_ndx( 'usr_OA_O2' )
    usr_XNO2NO3_M_ndx    = get_rxt_ndx( 'usr_XNO2NO3_M' )
    usr_NO2XNO3_M_ndx    = get_rxt_ndx( 'usr_NO2XNO3_M' )
    usr_XNO2NO3_aer_ndx  = get_rxt_ndx( 'usr_XNO2NO3_aer' )
    usr_NO2XNO3_aer_ndx  = get_rxt_ndx( 'usr_NO2XNO3_aer' )
    usr_XHNO3_OH_ndx     = get_rxt_ndx( 'usr_XHNO3_OH' )
    usr_XNO3_aer_ndx     = get_rxt_ndx( 'usr_XNO3_aer' )
    usr_XNO2_aer_ndx     = get_rxt_ndx( 'usr_XNO2_aer' )
    usr_MCO3_XNO2_ndx    = get_rxt_ndx( 'usr_MCO3_XNO2' )
    usr_XPAN_M_ndx       = get_rxt_ndx( 'usr_XPAN_M' )
    usr_XMPAN_M_ndx      = get_rxt_ndx( 'usr_XMPAN_M' )
    usr_XHO2NO2_M_ndx    = get_rxt_ndx( 'usr_XHO2NO2_M' )
!
! reduced hydrocarbon chemistry
!
    usr_C2O3_NO2_ndx     = get_rxt_ndx( 'usr_C2O3_NO2' )
    usr_C2H4_OH_ndx      = get_rxt_ndx( 'usr_C2H4_OH' )
    usr_XO2N_HO2_ndx     = get_rxt_ndx( 'usr_XO2N_HO2' )
    usr_C2O3_XNO2_ndx    = get_rxt_ndx( 'usr_C2O3_XNO2' )
!
    tag_XO2N_NO_ndx      = get_rxt_ndx( 'tag_XO2N_NO' )
    tag_XO2_HO2_ndx      = get_rxt_ndx( 'tag_XO2_HO2' )
    tag_XO2_NO_ndx       = get_rxt_ndx( 'tag_XO2_NO' )
!
! stratospheric chemistry
!
    usr_O_O_ndx          = get_rxt_ndx( 'usr_O_O' )
    usr_CL2O2_M_ndx      = get_rxt_ndx( 'usr_CL2O2_M' )
    usr_SO3_H2O_ndx      = get_rxt_ndx( 'usr_SO3_H2O' )
!
    tag_CLO_CLO_M_ndx      = get_rxt_ndx( 'tag_CLO_CLO_M' )
    if (tag_CLO_CLO_M_ndx<0) then ! for backwards compatibility
       tag_CLO_CLO_M_ndx   = get_rxt_ndx( 'tag_CLO_CLO' )
    endif
!
! reactions to remove BAM aerosols in the stratosphere
!
    usr_strat_tau_ndx( 1) = get_rxt_ndx( 'usr_CB1_strat_tau' )
    usr_strat_tau_ndx( 2) = get_rxt_ndx( 'usr_CB2_strat_tau' )
    usr_strat_tau_ndx( 3) = get_rxt_ndx( 'usr_OC1_strat_tau' )
    usr_strat_tau_ndx( 4) = get_rxt_ndx( 'usr_OC2_strat_tau' )
    usr_strat_tau_ndx( 5) = get_rxt_ndx( 'usr_SO4_strat_tau' )
    usr_strat_tau_ndx( 6) = get_rxt_ndx( 'usr_SOA_strat_tau' )
    usr_strat_tau_ndx( 7) = get_rxt_ndx( 'usr_NH4_strat_tau' )
    usr_strat_tau_ndx( 8) = get_rxt_ndx( 'usr_NH4NO3_strat_tau' )
    usr_strat_tau_ndx( 9) = get_rxt_ndx( 'usr_SSLT01_strat_tau' )
    usr_strat_tau_ndx(10) = get_rxt_ndx( 'usr_SSLT02_strat_tau' )
    usr_strat_tau_ndx(11) = get_rxt_ndx( 'usr_SSLT03_strat_tau' )
    usr_strat_tau_ndx(12) = get_rxt_ndx( 'usr_SSLT04_strat_tau' )
    usr_strat_tau_ndx(13) = get_rxt_ndx( 'usr_DST01_strat_tau' )
    usr_strat_tau_ndx(14) = get_rxt_ndx( 'usr_DST02_strat_tau' )
    usr_strat_tau_ndx(15) = get_rxt_ndx( 'usr_DST03_strat_tau' )
    usr_strat_tau_ndx(16) = get_rxt_ndx( 'usr_DST04_strat_tau' )
    usr_strat_tau_ndx(17) = get_rxt_ndx( 'usr_SO2t_strat_tau' )
    usr_strat_tau_ndx(18) = get_rxt_ndx( 'usr_SOAI_strat_tau' )
    usr_strat_tau_ndx(19) = get_rxt_ndx( 'usr_SOAM_strat_tau' )
    usr_strat_tau_ndx(20) = get_rxt_ndx( 'usr_SOAB_strat_tau' )
    usr_strat_tau_ndx(21) = get_rxt_ndx( 'usr_SOAT_strat_tau' )
    usr_strat_tau_ndx(22) = get_rxt_ndx( 'usr_SOAX_strat_tau' )
!
! stratospheric aerosol chemistry
!
    het1_ndx             = get_rxt_ndx( 'het1' )
!
! ion chemistry
!
    ion1_ndx  = get_rxt_ndx( 'ion_Op_O2' )
    ion2_ndx  = get_rxt_ndx( 'ion_Op_N2' )
    ion3_ndx  = get_rxt_ndx( 'ion_N2p_Oa' )
    ion11_ndx = get_rxt_ndx( 'ion_N2p_Ob' )

    elec1_ndx  = get_rxt_ndx( 'elec1' )
    elec2_ndx  = get_rxt_ndx( 'elec2' )
    elec3_ndx  = get_rxt_ndx( 'elec3' )

    do i = 1,nean
      write (xchar,'(i4)') i
      rxtname = 'ean'//trim(adjustl(xchar))
      ean_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,nrpe
      write (xchar,'(i4)') i
      rxtname = 'rpe'//trim(adjustl(xchar))
      rpe_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,npir
      write (xchar,'(i4)') i
      rxtname = 'pir'//trim(adjustl(xchar))
      pir_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,nedn
      write (xchar,'(i4)') i
      rxtname = 'edn'//trim(adjustl(xchar))
      edn_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,nnir
      write (xchar,'(i4)') i
      rxtname = 'nir'//trim(adjustl(xchar))
      nir_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,niira
      write (xchar,'(i4)') i
      rxtname = 'iira'//trim(adjustl(xchar))
      iira_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    do i = 1,niirb
      write (xchar,'(i4)') i
      rxtname = 'iirb'//trim(adjustl(xchar))
      iirb_ndx(i) = get_rxt_ndx(trim(rxtname))
    enddo

    usr_clm_h2o_m_ndx = get_rxt_ndx( 'usr_CLm_H2O_M' )
    usr_clm_hcl_m_ndx = get_rxt_ndx( 'usr_CLm_HCL_M' )

    elec4_ndx  = get_rxt_ndx( 'Op2P_ea' )
    elec5_ndx  = get_rxt_ndx( 'Op2P_eb' )
    elec6_ndx  = get_rxt_ndx( 'Op2D_e' )

    has_ion_rxts = ion1_ndx>0 .and. ion2_ndx>0 .and. ion3_ndx>0 .and. elec1_ndx>0 &
                 .and. elec2_ndx>0 .and. elec3_ndx>0

    has_d_chem = &
         all(ean_ndx>0) .and. &
         all(rpe_ndx>0) .and. &
         all(pir_ndx>0) .and. &
         all(edn_ndx>0) .and. &
         all(nir_ndx>0) .and. &
         all(iira_ndx>0) .and. &
         all(iirb_ndx>0) .and. &
         usr_clm_h2o_m_ndx>0 .and. usr_clm_hcl_m_ndx>0

    h2o_ndx    = get_spc_ndx( 'H2O' )

    !
    ! llnl super fast
    !
    usr_oh_co_ndx  = get_rxt_ndx( 'usr_oh_co' )
    het_no2_h2o_ndx  = get_rxt_ndx( 'het_no2_h2o' )
    usr_oh_dms_ndx  = get_rxt_ndx( 'usr_oh_dms' )
    aq_so2_h2o2_ndx  = get_rxt_ndx( 'aq_so2_h2o2' )
    aq_so2_o3_ndx  = get_rxt_ndx( 'aq_so2_o3' )
    
    ! write(iulog,*) 'fkmmm: begin getting ndx'

    ! -----------------------------------    
  	! WSY for Phase Transfer: phase-transfer on wet aerosols
  	! -----------------------------------
  	g2a_SO2_a1_ndx  = get_rxt_ndx( 'g2a_SO2_a1' )
  	g2a_SO2_a2_ndx  = get_rxt_ndx( 'g2a_SO2_a2' )
  	g2a_SO2_a3_ndx  = get_rxt_ndx( 'g2a_SO2_a3' )
  	a2g_SO2_a1_ndx  = get_rxt_ndx( 'a2g_SO2_a1' )
  	a2g_SO2_a2_ndx  = get_rxt_ndx( 'a2g_SO2_a2' )
  	a2g_SO2_a3_ndx  = get_rxt_ndx( 'a2g_SO2_a3' )
  	g2a_H2O2_a1_ndx  = get_rxt_ndx( 'g2a_H2O2_a1' )
  	g2a_H2O2_a2_ndx  = get_rxt_ndx( 'g2a_H2O2_a2' )
  	g2a_H2O2_a3_ndx  = get_rxt_ndx( 'g2a_H2O2_a3' )
  	a2g_H2O2_a1_ndx  = get_rxt_ndx( 'a2g_H2O2_a1' )
  	a2g_H2O2_a2_ndx  = get_rxt_ndx( 'a2g_H2O2_a2' )
  	a2g_H2O2_a3_ndx  = get_rxt_ndx( 'a2g_H2O2_a3' )
  	alwc_SIV_H2O2_a1_ndx  = get_rxt_ndx( 'alwc_SIV_H2O2_a1' )
  	alwc_SIV_H2O2_a2_ndx  = get_rxt_ndx( 'alwc_SIV_H2O2_a2' )
  	alwc_SIV_H2O2_a3_ndx  = get_rxt_ndx( 'alwc_SIV_H2O2_a3' )
    
    ! -----------------------------------    
    ! fkm for Phase Transfer: phase-transfer on wet aerosols
    ! -----------------------------------  
    
    g2a_OH_a1_ndx  = get_rxt_ndx( 'g2a_OH_a1' )
    g2a_OH_a2_ndx  = get_rxt_ndx( 'g2a_OH_a2' )
    g2a_OH_a3_ndx  = get_rxt_ndx( 'g2a_OH_a3' )
    a2g_OH_a1_ndx  = get_rxt_ndx( 'a2g_OH_a1' )
    a2g_OH_a2_ndx  = get_rxt_ndx( 'a2g_OH_a2' )
    a2g_OH_a3_ndx  = get_rxt_ndx( 'a2g_OH_a3' )
    g2a_O3_a1_ndx  = get_rxt_ndx( 'g2a_O3_a1' )
    g2a_O3_a2_ndx  = get_rxt_ndx( 'g2a_O3_a2' )
    g2a_O3_a3_ndx  = get_rxt_ndx( 'g2a_O3_a3' )
    a2g_O3_a1_ndx  = get_rxt_ndx( 'a2g_O3_a1' )
    a2g_O3_a2_ndx  = get_rxt_ndx( 'a2g_O3_a2' )
    a2g_O3_a3_ndx  = get_rxt_ndx( 'a2g_O3_a3' )
    
    g2a_DMS_a1_ndx  = get_rxt_ndx( 'g2a_DMS_a1' )
    g2a_DMS_a2_ndx  = get_rxt_ndx( 'g2a_DMS_a2' )
    g2a_DMS_a3_ndx  = get_rxt_ndx( 'g2a_DMS_a3' )
    a2g_DMS_a1_ndx  = get_rxt_ndx( 'a2g_DMS_a1' )
    a2g_DMS_a2_ndx  = get_rxt_ndx( 'a2g_DMS_a2' )
    a2g_DMS_a3_ndx  = get_rxt_ndx( 'a2g_DMS_a3' )
    g2a_DMSO_a1_ndx  = get_rxt_ndx( 'g2a_DMSO_a1' )
    g2a_DMSO_a2_ndx  = get_rxt_ndx( 'g2a_DMSO_a2' )
    g2a_DMSO_a3_ndx  = get_rxt_ndx( 'g2a_DMSO_a3' )
    a2g_DMSO_a1_ndx  = get_rxt_ndx( 'a2g_DMSO_a1' )
    a2g_DMSO_a2_ndx  = get_rxt_ndx( 'a2g_DMSO_a2' )
    a2g_DMSO_a3_ndx  = get_rxt_ndx( 'a2g_DMSO_a3' )
    g2a_MSIA_a1_ndx  = get_rxt_ndx( 'g2a_MSIA_a1' )
    g2a_MSIA_a2_ndx  = get_rxt_ndx( 'g2a_MSIA_a2' )
    g2a_MSIA_a3_ndx  = get_rxt_ndx( 'g2a_MSIA_a3' )
    a2g_MSIA_a1_ndx  = get_rxt_ndx( 'a2g_MSIA_a1' )
    a2g_MSIA_a2_ndx  = get_rxt_ndx( 'a2g_MSIA_a2' )
    a2g_MSIA_a3_ndx  = get_rxt_ndx( 'a2g_MSIA_a3' )
    g2a_MSA_a1_ndx  = get_rxt_ndx( 'g2a_MSA_a1' )
    g2a_MSA_a2_ndx  = get_rxt_ndx( 'g2a_MSA_a2' )
    g2a_MSA_a3_ndx  = get_rxt_ndx( 'g2a_MSA_a3' )
    a2g_MSA_a1_ndx  = get_rxt_ndx( 'a2g_MSA_a1' )
    a2g_MSA_a2_ndx  = get_rxt_ndx( 'a2g_MSA_a2' )
    a2g_MSA_a3_ndx  = get_rxt_ndx( 'a2g_MSA_a3' )
    
    DMS_O3_a1_ndx   = get_rxt_ndx( 'DMS_O3_a1' )
    DMSO_OH_a1_ndx  = get_rxt_ndx( 'DMSO_OH_a1' )
    MSIA_OH_a1_ndx  = get_rxt_ndx( 'MSIA_OH_a1' )
    MSIA_O3_a1_ndx  = get_rxt_ndx( 'MSIA_O3_a1' )
    MSA_OH_a1_ndx  = get_rxt_ndx( 'MSA_OH_a1' )
    DMS_O3_a2_ndx   = get_rxt_ndx( 'DMS_O3_a2' )
    DMSO_OH_a2_ndx  = get_rxt_ndx( 'DMSO_OH_a2' )
    MSIA_OH_a2_ndx  = get_rxt_ndx( 'MSIA_OH_a2' )
    MSIA_O3_a2_ndx  = get_rxt_ndx( 'MSIA_O3_a2' )
    MSA_OH_a2_ndx  = get_rxt_ndx( 'MSA_OH_a2' )
    DMS_O3_a3_ndx   = get_rxt_ndx( 'DMS_O3_a3' )
    DMSO_OH_a3_ndx  = get_rxt_ndx( 'DMSO_OH_a3' )
    MSIA_OH_a3_ndx  = get_rxt_ndx( 'MSIA_OH_a3' )
    MSIA_O3_a3_ndx  = get_rxt_ndx( 'MSIA_O3_a3' )
    MSA_OH_a3_ndx  = get_rxt_ndx( 'MSA_OH_a3' )
    
    g2a_HOBR_a1_ndx  = get_rxt_ndx( 'g2a_HOBR_a1' )
    g2a_HOBR_a2_ndx  = get_rxt_ndx( 'g2a_HOBR_a2' )
    g2a_HOBR_a3_ndx  = get_rxt_ndx( 'g2a_HOBR_a3' )
    a2g_HOBR_a1_ndx  = get_rxt_ndx( 'a2g_HOBR_a1' )
    a2g_HOBR_a2_ndx  = get_rxt_ndx( 'a2g_HOBR_a2' )
    a2g_HOBR_a3_ndx  = get_rxt_ndx( 'a2g_HOBR_a3' )
    SIV_HOBR_a1_ndx  = get_rxt_ndx( 'SIV_HOBR_a1' )
    SIV_HOBR_a2_ndx  = get_rxt_ndx( 'SIV_HOBR_a2' )
    SIV_HOBR_a3_ndx  = get_rxt_ndx( 'SIV_HOBR_a3' )
    SIV_O3_a1_ndx  = get_rxt_ndx( 'SIV_O3_a1' )
    SIV_O3_a2_ndx  = get_rxt_ndx( 'SIV_O3_a2' )
    SIV_O3_a3_ndx  = get_rxt_ndx( 'SIV_O3_a3' )
    
    ! -----------------------------------    
    ! fkm for Phase Transfer: phase-transfer on cloud
    ! -----------------------------------  
    g2c_DMS_c_ndx  = get_rxt_ndx( 'g2c_DMS_c' )
    c2g_DMS_c_ndx  = get_rxt_ndx( 'c2g_DMS_c' )
    g2c_DMSO_c_ndx  = get_rxt_ndx( 'g2c_DMSO_c' )
    c2g_DMSO_c_ndx  = get_rxt_ndx( 'c2g_DMSO_c' )
    g2c_MSIA_c_ndx  = get_rxt_ndx( 'g2c_MSIA_c' )
    c2g_MSIA_c_ndx  = get_rxt_ndx( 'c2g_MSIA_c' )
    g2c_MSA_c_ndx  = get_rxt_ndx( 'g2c_MSA_c' )
    c2g_MSA_c_ndx  = get_rxt_ndx( 'c2g_MSA_c' )
    g2c_OH_c_ndx  = get_rxt_ndx( 'g2c_OH_c' )
    c2g_OH_c_ndx  = get_rxt_ndx( 'c2g_OH_c' )
    g2c_O3_c_ndx  = get_rxt_ndx( 'g2c_O3_c' )
    c2g_O3_c_ndx  = get_rxt_ndx( 'c2g_O3_c' )
    
    DMS_O3_c_ndx   = get_rxt_ndx( 'DMS_O3_c' )
    DMSO_OH_c_ndx  = get_rxt_ndx( 'DMSO_OH_c' )
    MSIA_OH_c_ndx  = get_rxt_ndx( 'MSIA_OH_c' )
    MSIA_O3_c_ndx  = get_rxt_ndx( 'MSIA_O3_c' )
    MSA_OH_c_ndx  = get_rxt_ndx( 'MSA_OH_c' )

!lke++
! CO tags
!
    usr_COhc_OH_ndx      = get_rxt_ndx( 'usr_COhc_OH' )
    usr_COme_OH_ndx      = get_rxt_ndx( 'usr_COme_OH' )
    usr_CO01_OH_ndx      = get_rxt_ndx( 'usr_CO01_OH' )
    usr_CO02_OH_ndx      = get_rxt_ndx( 'usr_CO02_OH' )
    usr_CO03_OH_ndx      = get_rxt_ndx( 'usr_CO03_OH' )
    usr_CO04_OH_ndx      = get_rxt_ndx( 'usr_CO04_OH' )
    usr_CO05_OH_ndx      = get_rxt_ndx( 'usr_CO05_OH' )
    usr_CO06_OH_ndx      = get_rxt_ndx( 'usr_CO06_OH' )
    usr_CO07_OH_ndx      = get_rxt_ndx( 'usr_CO07_OH' )
    usr_CO08_OH_ndx      = get_rxt_ndx( 'usr_CO08_OH' )
    usr_CO09_OH_ndx      = get_rxt_ndx( 'usr_CO09_OH' )
    usr_CO10_OH_ndx      = get_rxt_ndx( 'usr_CO10_OH' )
    usr_CO11_OH_ndx      = get_rxt_ndx( 'usr_CO11_OH' )
    usr_CO12_OH_ndx      = get_rxt_ndx( 'usr_CO12_OH' )
    usr_CO13_OH_ndx      = get_rxt_ndx( 'usr_CO13_OH' )
    usr_CO14_OH_ndx      = get_rxt_ndx( 'usr_CO14_OH' )
    usr_CO15_OH_ndx      = get_rxt_ndx( 'usr_CO15_OH' )
    usr_CO16_OH_ndx      = get_rxt_ndx( 'usr_CO16_OH' )
    usr_CO17_OH_ndx      = get_rxt_ndx( 'usr_CO17_OH' )
    usr_CO18_OH_ndx      = get_rxt_ndx( 'usr_CO18_OH' )
    usr_CO19_OH_ndx      = get_rxt_ndx( 'usr_CO19_OH' )
    usr_CO20_OH_ndx      = get_rxt_ndx( 'usr_CO20_OH' )
    usr_CO21_OH_ndx      = get_rxt_ndx( 'usr_CO21_OH' )
    usr_CO22_OH_ndx      = get_rxt_ndx( 'usr_CO22_OH' )
    usr_CO23_OH_ndx      = get_rxt_ndx( 'usr_CO23_OH' )
    usr_CO24_OH_ndx      = get_rxt_ndx( 'usr_CO24_OH' )
    usr_CO25_OH_ndx      = get_rxt_ndx( 'usr_CO25_OH' )
    usr_CO26_OH_ndx      = get_rxt_ndx( 'usr_CO26_OH' )
    usr_CO27_OH_ndx      = get_rxt_ndx( 'usr_CO27_OH' )
    usr_CO28_OH_ndx      = get_rxt_ndx( 'usr_CO28_OH' )
    usr_CO29_OH_ndx      = get_rxt_ndx( 'usr_CO29_OH' )
    usr_CO30_OH_ndx      = get_rxt_ndx( 'usr_CO30_OH' )
    usr_CO31_OH_ndx      = get_rxt_ndx( 'usr_CO31_OH' )
    usr_CO32_OH_ndx      = get_rxt_ndx( 'usr_CO32_OH' )
    usr_CO33_OH_ndx      = get_rxt_ndx( 'usr_CO33_OH' )
    usr_CO34_OH_ndx      = get_rxt_ndx( 'usr_CO34_OH' )
    usr_CO35_OH_ndx      = get_rxt_ndx( 'usr_CO35_OH' )
    usr_CO36_OH_ndx      = get_rxt_ndx( 'usr_CO36_OH' )
    usr_CO37_OH_ndx      = get_rxt_ndx( 'usr_CO37_OH' )
    usr_CO38_OH_ndx      = get_rxt_ndx( 'usr_CO38_OH' )
    usr_CO39_OH_ndx      = get_rxt_ndx( 'usr_CO39_OH' )
    usr_CO40_OH_ndx      = get_rxt_ndx( 'usr_CO40_OH' )
    usr_CO41_OH_ndx      = get_rxt_ndx( 'usr_CO41_OH' )
    usr_CO42_OH_ndx      = get_rxt_ndx( 'usr_CO42_OH' )
!lke--

    if (masterproc) then
       write(iulog,*) ' '
       write(iulog,*) 'usrrxt_inti: diagnostics '
       write(iulog,'(10i5)') usr_O_O2_ndx,usr_HO2_HO2_ndx,tag_NO2_NO3_ndx,usr_N2O5_M_ndx,tag_NO2_OH_ndx,usr_HNO3_OH_ndx &
                            ,tag_NO2_HO2_ndx,usr_HO2NO2_M_ndx,usr_N2O5_aer_ndx,usr_NO3_aer_ndx,usr_NO2_aer_ndx &
                            ,usr_CO_OH_b_ndx,tag_C2H4_OH_ndx,tag_C3H6_OH_ndx,tag_CH3CO3_NO2_ndx,usr_PAN_M_ndx,usr_CH3COCH3_OH_ndx &
                            ! ,usr_MCO3_NO2_ndx,usr_MPAN_M_ndx,usr_XOOH_OH_ndx,usr_SO2_OH_ndx, usr_DMS_OH_ndx, usr_HO2_aer_ndx &    ! commented by fkm for DMS intermediates
                            ,usr_MCO3_NO2_ndx,usr_MPAN_M_ndx,usr_XOOH_OH_ndx,usr_SO2_OH_ndx, usr_HO2_aer_ndx &  ! fkm for DMS intermediates
                            ,usr_GLYOXAL_aer_ndx,usr_ISOPNITA_aer_ndx,usr_ISOPNITB_aer_ndx,usr_ONITR_aer_ndx,usr_HONITR_aer_ndx &
                            ,usr_TERPNIT_aer_ndx,usr_NTERPOOH_aer_ndx,usr_NC4CHO_aer_ndx,usr_NC4CH2OH_aer_ndx

    end if

  end subroutine usrrxt_inti

  subroutine usrrxt( rxt, temp, tempi, tempe, invariants, h2ovmr,  &
                     pmid, m, sulfate, mmr, relhum, strato_sad, &
                     cldfrc, &          ! added by fkm for non-cloud sulfate 
                     xphlwc, cloud_ph, &         ! added by fkm for cloud reactions
                     tropchemlev, dlat, ncol, sad_trop, reff_trop, cwat, mbar, pbuf )

!-----------------------------------------------------------------
!        ... set the user specified reaction rates
!-----------------------------------------------------------------

    use mo_chem_utls,   only : get_rxt_ndx, get_spc_ndx        ! fkm for Phase Transfer; added two new functions to get radius
    use mo_constants,  only : pi, avo => avogadro, boltz_cgs, rgas
    use chem_mods,     only : nfs, rxntot, gas_pcnst, inv_m_ndx=>indexm
    use mo_setinv,     only : inv_o2_ndx=>o2_ndx, inv_h2o_ndx=>h2o_ndx
    ! use physics_buffer,only : physics_buffer_desc     ! commended by fkm for Phase Transfer;
    use physics_buffer,only : physics_buffer_desc, pbuf_get_index, pbuf_get_field        ! fkm for Phase Transfer; added two new functions to get radius
    use carma_flags_mod, only : carma_hetchem_feedback
    use aero_model,      only : aero_model_surfarea
    use rad_constituents,only : rad_cnst_get_info
    
    use modal_aero_data, only : MOSAIC_pH, MOSAIC_HSO4, &
                                MOSAIC_SO4, MOSAIC_NO3  ! dsj; fkm for MOSAIC pH

    implicit none

!-----------------------------------------------------------------
!        ... dummy arguments
!-----------------------------------------------------------------
    integer :: dgnumwet_idx                    ! fkm for Phase Transfer; 

    integer, intent(in)     :: ncol
    integer, intent(in)     :: tropchemlev(pcols)         ! trop/strat reaction separation vertical index
    real(r8), intent(in)    :: dlat(:)                    ! degrees latitude
    real(r8), intent(in)    :: temp(pcols,pver)           ! temperature (K); neutral temperature
    real(r8), intent(in)    :: tempi(pcols,pver)          ! ionic temperature (K); only used if ion chemistry
    real(r8), intent(in)    :: tempe(pcols,pver)          ! electronic temperature (K); only used if ion chemistry
    real(r8), intent(in)    :: m(ncol,pver)               ! total atm density (/cm^3)
    real(r8), intent(in)    :: sulfate(ncol,pver)         ! sulfate aerosol (mol/mol)
    real(r8), intent(in)    :: strato_sad(pcols,pver)     ! stratospheric aerosol sad (1/cm)
    real(r8), intent(in)    :: h2ovmr(ncol,pver)          ! water vapor (mol/mol)
    real(r8), intent(in)    :: relhum(ncol,pver)          ! relative humidity
    real(r8), intent(in)    :: pmid(pcols,pver)           ! midpoint pressure (Pa)
    real(r8), intent(in)    :: invariants(ncol,pver,nfs)  ! invariants density (/cm^3)
    real(r8), intent(in)    :: mmr(pcols,pver,gas_pcnst)  ! species concentrations (kg/kg)
    real(r8), intent(in)    :: cwat(ncol,pver) !PJC Condensed Water (liquid+ice) (kg/kg)
    real(r8), intent(in)    :: mbar(ncol,pver) !PJC Molar mass of air (g/mol)
    real(r8), intent(inout) :: rxt(ncol,pver,rxntot)      ! gas phase rates
    real(r8), intent(out)   :: sad_trop(pcols,pver)       ! tropospheric surface area density (cm2/cm3)
    real(r8), intent(out)   :: reff_trop(pcols,pver)      ! tropospheric effective radius (cm)
    type(physics_buffer_desc), pointer :: pbuf(:)
    
    real(r8), intent(in)    :: cldfrc(pcols,pver)       ! cloud fraction; added by fkm for non-cloud sulfate 
    real(r8), intent(inout)    :: xphlwc(pcols,pver)       ! cloud ph * LWC; added by fkm for cloud reactions
    real(r8), intent(inout)    :: cloud_ph(pcols,pver)       ! cloud ph; added by fkm for cloud reactions

!-----------------------------------------------------------------
!        ... local variables
!-----------------------------------------------------------------

    real(r8), parameter :: dg = 0.1_r8            ! mole diffusion =0.1 cm2/s (Dentener, 1993)

!-----------------------------------------------------------------
! 	... reaction probabilities for heterogeneous reactions
!-----------------------------------------------------------------
    real(r8), parameter :: gamma_n2o5 = 0.10_r8         ! from Jacob, Atm Env, 34, 2131, 2000
    real(r8), parameter :: gamma_ho2  = 0.20_r8         !
    real(r8), parameter :: gamma_no2  = 0.0001_r8       !
    real(r8), parameter :: gamma_no3  = 0.001_r8        !
    real(r8), parameter :: gamma_glyoxal  = 2.0e-4_r8   !  Washenfelder et al, JGR, 2011
!TS1 species
    real(r8), parameter :: gamma_isopnita  = 0.005_r8        ! from Fisher et al., ACP, 2016
    real(r8), parameter :: gamma_isopnitb  = 0.005_r8        !
    real(r8), parameter :: gamma_onitr     = 0.005_r8        !
    real(r8), parameter :: gamma_honitr    = 0.005_r8        !
    real(r8), parameter :: gamma_terpnit   = 0.01_r8         !
    real(r8), parameter :: gamma_nterpooh  = 0.01_r8         !
    real(r8), parameter :: gamma_nc4cho    = 0.005_r8        !
    real(r8), parameter :: gamma_nc4ch2oh  = 0.005_r8        !


    integer  ::  i, k
    integer  ::  l
    real(r8) ::  tp(ncol)                       ! 300/t
    real(r8) ::  tinv(ncol)                     ! 1/t
    real(r8) ::  ko(ncol)
    real(r8) ::  term1(ncol)
    real(r8) ::  term2(ncol)
    real(r8) ::  kinf(ncol)
    real(r8) ::  fc(ncol)
    real(r8) ::  xr(ncol)
    real(r8) ::  sur(ncol)
    real(r8) ::  sqrt_t(ncol)                   ! sqrt( temp )
    real(r8) ::  sqrt_t_58(ncol)                ! sqrt( temp / 58.)
    real(r8) ::  exp_fac(ncol)                  ! vector exponential
    real(r8) ::  lwc(ncol)
    real(r8) ::  ko_m(ncol)
    real(r8) ::  k0(ncol)
    real(r8) ::  kinf_m(ncol)
    real(r8) ::  o2(ncol)
    real(r8) ::  c_n2o5, c_ho2, c_no2, c_no3, c_glyoxal
!TS1 species
    real(r8) ::  c_isopnita, c_isopnitb, c_onitr, c_honitr, c_terpnit, c_nterpooh
    real(r8) ::  c_nc4cho, c_nc4ch2oh
    
    ! real(r8) ::  temp_1d (ncol)                 ! added by fkm for Phase Tranfser; temperature (K).
    integer  :: id_aq_c                           ! added by fkm for cloud reactions
    ! real(r8) ::  cloud_ph(ncol)                 ! added by fkm for cloud reactions 
    real(r8), dimension(ncol) :: &
                  faq_DMS, &              ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  faq_DMSO, &                 ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  faq_MSIA, &                 ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  faq_MSA, &                 ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  faq_OH, &                 ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  faq_O3, &                 ! added by fkm for cloud reactions; distribution factor from Seinfeld Eq 7.26
                  he_DMS, &                 ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  he_DMSO, &                 ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  he_MSIA, &                 ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  he_MSA, &                 ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  he_OH, &                 ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  he_O3                    ! added by fkm for cloud reactions; Henry's Law constant from Seinfeld Eq 7.26
                  
    real(r8) :: radius_cm_c(ncol)                 ! added by fkm for cloud reactions; cloud radius in [m]
    real(r8) :: alwc_vol_vol_c(ncol)             ! added by fkm for cloud reactions; cloud vol-water to vol-air
    
    real(r8), dimension(ncol) :: &
                  kt_DMS_a1,  kt_DMS_a2,  kt_DMS_a3,  &    ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  kt_DMSO_a1, kt_DMSO_a2, kt_DMSO_a3, &    ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  kt_MSIA_a1, kt_MSIA_a2, kt_MSIA_a3, &    ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  kt_MSA_a1,  kt_MSA_a2,  kt_MSA_a3,  &    ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  kt_OH_a1,   kt_OH_a2,   kt_OH_a3,   &    ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  kt_O3_a1,   kt_O3_a2,   kt_O3_a3         ! added by fkm for cloud reactions; phase transfer constant from Seinfeld Eq 7.26
                  
                  
    real(r8) ::  heff_M_atm_c(ncol)        ! added by fkm for cloud reactions; effective Henry's Law Constant
    real(r8) ::  conv_aq_rate_coeff_c(ncol)   ! unit converter based on LWC for cloud reactions [M s-1 -> (molecules cm-3 s-1)]
    real(r8) ::  cloud_fraction(ncol)       ! cloud fraction; added by fkm for cloud reactions
 
    real(r8) ::  amas
    !-----------------------------------------------------------------
    !	... density of sulfate aerosol
    !-----------------------------------------------------------------
    real(r8), parameter :: gam1 = 0.04_r8                 ! N2O5+SUL ->2HNO3
    real(r8), parameter :: wso4 = 98._r8
    real(r8), parameter :: den  = 1.15_r8                 ! each molecule of SO4(aer) density g/cm3
    !-------------------------------------------------
    ! 	... volume of sulfate particles
    !           assuming mean rm
    !           continient 0.05um  0.07um  0.09um
    !           ocean      0.09um  0.25um  0.37um
    !                      0.16um                  Blake JGR,7195, 1995
    !-------------------------------------------------
    real(r8), parameter :: rm1  = 0.16_r8*1.e-4_r8             ! mean radii in cm
    real(r8), parameter :: fare = 4._r8*pi*rm1*rm1             ! each mean particle(r=0.1u) area   cm2/cm3

    !-----------------------------------------------------------------------
    !        ... Aqueous phase sulfur quantities for SO2 + H2O2 and SO2 + O3
    !-----------------------------------------------------------------------
    real(r8), parameter  :: HENRY298_H2O2 =  7.45e+04_r8
    real(r8), parameter  :: H298_H2O2     = -1.45e+04_r8
    real(r8), parameter  :: HENRY298_SO2  =  1.23e+00_r8
    real(r8), parameter  :: H298_SO2      = -6.25e+03_r8
    real(r8), parameter  :: K298_SO2_HSO3 =  1.3e-02_r8
    real(r8), parameter  :: H298_SO2_HSO3 = -4.16e+03_r8
    real(r8), parameter  :: R_CONC        =  82.05e+00_r8 / avo
    real(r8), parameter  :: R_CAL         =  rgas * 0.239006e+00_r8
    real(r8), parameter  :: K_AQ          =  7.57e+07_r8
    real(r8), parameter  :: ER_AQ         =  4.43e+03_r8

    real(r8), parameter  :: HENRY298_O3   =  1.13e-02_r8
    real(r8), parameter  :: H298_O3       = -5.04e+03_r8
    real(r8), parameter  :: K298_HSO3_SO3 =  6.6e-08_r8
    real(r8), parameter  :: H298_HSO3_SO3 = -2.23e+03_r8
    real(r8), parameter  :: K0_AQ         =  2.4e+04_r8
    real(r8), parameter  :: ER0_AQ        =  0.0e+00_r8
    real(r8), parameter  :: K1_AQ         =  3.7e+05_r8
    real(r8), parameter  :: ER1_AQ        =  5.53e+03_r8
    real(r8), parameter  :: K2_AQ         =  1.5e+09_r8
    real(r8), parameter  :: ER2_AQ        =  5.28e+03_r8

    real(r8), parameter  :: pH            =  4.5e+00_r8
    
    real(r8), parameter  :: R_atm_per_K_per_M = 0.08205_r8  ! fkm for Phase Transit; gas constant [L atm mol^-1 K^-1 or atm M^-1 K^-1]

    real(r8), pointer :: sfc(:), dm_aer(:)
    integer :: ntot_amode
    
    ! -------------------------------    
    ! WSY for Phase Transfer: dummies for phase-transfer
    ! -------------------------------
    integer   :: id_gas, id_aq_a1, id_aq_a2, id_aq_a3
    real(r8)  :: MW_gas_kg_mol, mass_accommodation_coeff
    real(r8)  :: thermalspeed_cm_s(ncol)
    real(r8)  :: radius_cm_a1(ncol)
    real(r8)  :: radius_cm_a2(ncol)
    real(r8)  :: radius_cm_a3(ncol)
    real(r8)  :: alwc_vol_vol_a1(ncol)
    real(r8)  :: alwc_vol_vol_a2(ncol)
    real(r8)  :: alwc_vol_vol_a3(ncol)
    real(r8)  :: heff_M_atm_a1(ncol)
    real(r8)  :: heff_M_atm_a2(ncol)
    real(r8)  :: heff_M_atm_a3(ncol)
    real(r8)  :: pH_a1(ncol)
    real(r8)  :: pH_a2(ncol)
    real(r8)  :: pH_a3(ncol)
    real(r8)  :: conv_aq_rate_coeff_a1(ncol)
    real(r8)  :: conv_aq_rate_coeff_a2(ncol)
    real(r8)  :: conv_aq_rate_coeff_a3(ncol)
  
    real(r8)  :: alwc_vol_vol(ncol)       ! fkm for Phase Transit; aerosol Liquid Water Content (cm^3-water / cm^3-air)
    real(r8), pointer :: qaerwat(:,:,:)   ! aerosol water-air mass ratio?; fkm for MOSAIC aLWC (kg/kg)
    real(r8), pointer :: pH_buf(:,:,:)    ! aerosol pH; fkm for MOSAIC pH
    
    real(r8), pointer :: drop_radius_um(:,:) ! fkm for cloud reactions; effective cloud drop radius (um)
    
    character(len=16) :: SpeciesName	

    real(r8), pointer :: sfc_array(:,:,:), dm_array(:,:,:)

    real(r8), pointer :: dgncur_awet(:,:,:)   ! fkm for Phase Transit; dgncur_awet = geometric mean wet diameter for number distribution (m)

    ! get info about the modal aerosols
    ! get ntot_amode
    call rad_cnst_get_info(0, nmodes=ntot_amode)
    
    dgnumwet_idx    = pbuf_get_index('DGNUMWET')
    call pbuf_get_field(pbuf, dgnumwet_idx,   dgncur_awet, start=(/1,1,1/), kount=(/pcols,pver,ntot_amode/) )   ! fkm for Phase Transit; getting the dgncur_awet
    
    !!! ====== [BEG: fkm for MOSAIC aLWC] =====
    qaerwat_idx    = pbuf_get_index('QAERWAT')
    call pbuf_get_field(pbuf, qaerwat_idx,        qaerwat,  start=(/1,1,1/), kount=(/pcols,pver,ntot_amode/) )  ! getting aerosol water mass ratio
    !!! ====== [END: fkm for MOSAIC aLWC] =====
    
    !!! ====== [BEG: fkm for MOSAIC pH] =====
    pH_idx = pbuf_get_index('MOSAIC_pH')
    call pbuf_get_field(pbuf, pH_idx, pH_buf,  start=(/1,1,1/), kount=(/pcols,pver,ntot_amode/) )
    !!! ====== [END: fkm for MOSAIC pH] =====
    
    !!! ====== [BEG: fkm for cloud reactions] =====
    drop_radius_idx    = pbuf_get_index('REL')
    call pbuf_get_field(pbuf, drop_radius_idx,        drop_radius_um,  start=(/1,1/), kount=(/pcols,pver/) )  ! getting effective cloud drop radius (liq) (um)
    !!! ====== [END: fkm for cloud reaction] =====

    if (ntot_amode>0) then
       allocate(sfc_array(pcols,pver,ntot_amode), dm_array(pcols,pver,ntot_amode) )
    else
       allocate(sfc_array(pcols,pver,5), dm_array(pcols,pver,5) )
    endif

    sfc_array(:,:,:) = 0._r8
    dm_array(:,:,:) = 0._r8
    sad_trop(:,:) = 0._r8
    reff_trop(:,:) = 0._r8

    if( usr_NO2_aer_ndx > 0 .or. usr_NO3_aer_ndx > 0 .or. usr_N2O5_aer_ndx > 0 .or. usr_HO2_aer_ndx > 0 ) then ! commended by fkm for Phase Transfer

! sad_trop should be set outside of usrrxt ??
       if( carma_hetchem_feedback ) then
          sad_trop(:ncol,:pver)=strato_sad(:ncol,:pver)
       else

          call aero_model_surfarea( &
               mmr, rm1, relhum, pmid, temp, strato_sad, sulfate, m, tropchemlev, dlat, &
               het1_ndx, pbuf, ncol, sfc_array, dm_array, sad_trop, reff_trop )

       endif
    endif   ! commended by fkm for Phase Transfer

    level_loop : do k = 1,pver
       tinv(:)           = 1._r8 / temp(:ncol,k)
       tp(:)             = 300._r8 * tinv(:)
       sqrt_t(:)         = sqrt( temp(:ncol,k) )
       sqrt_t_58(:)      = sqrt( temp(:ncol,k) / 58.0_r8 )
       
       ! write(iulog,*) 'fkmmm: starts of the new codes'
       ! temp_1d(:)        = temp (:ncol,k)    ! added by fkm for Phase Transfer; 1-D temperature
       
       ! ===================================
       ! fkm for cloud reactions
       ! ===================================
       do i = 1,ncol
         
        if ( cloud_ph(i,k) .lt. 0._r8 .or. cloud_ph(i,k) .gt. 14._r8 ) then
          cloud_ph(i,k)       = 4.5_r8   ! fkm: cwat = wL cloud LWc (L of water / L of air)
        end if
        
       end do
       
       lwc(:) = cwat(:ncol,k) * invariants(:ncol,k,inv_m_ndx) / avo * mbar(:ncol,k) !PJC convert kg-water/kg-air to g-water/cm3-air (also = w_L (vol-water/vol-air) because 1 g = 1 cm3 of water)
       ! write(iulog,*) 'fkmmm: Phase Transfer; before LWC', lwc
       
       alwc_vol_vol_c(:) = max( 1.e-12_r8, cwat(:ncol,k) * invariants(:ncol,k,inv_m_ndx) / avo * mbar(:ncol,k))
       
       
       conv_aq_rate_coeff_c(:)  = 1.e3_r8 / (avo * max(1.e-12_r8, alwc_vol_vol_c(:))) ! 1.e3_r8 / (avo * max(small_value, lwc(:))) 

       ! ===================================
       ! WSY for Phase Transfer: phase-transfer on wet aerosols
       ! Siyuan Wang (siyuan@ucar.edu)
       ! ===================================
       
       ! fkm qaerwat
       alwc_vol_vol_a1(:) = max( 1.e-12_r8, qaerwat(:ncol, k, 1) * invariants(:ncol,k,inv_m_ndx) / avo * mbar(:ncol,k) ) ! fkm for Phase Transfer; use small_value to avoid 1/0 = NaN
       alwc_vol_vol_a2(:) = max( 1.e-12_r8, qaerwat(:ncol, k, 2) * invariants(:ncol,k,inv_m_ndx) / avo * mbar(:ncol,k) ) ! fkm for Phase Transfer; use small_value to avoid 1/0 = NaN
       alwc_vol_vol_a3(:) = max( 1.e-12_r8, qaerwat(:ncol, k, 3) * invariants(:ncol,k,inv_m_ndx) / avo * mbar(:ncol,k) ) ! fkm for Phase Transfer; use small_value to avoid 1/0 = NaN
       
       radius_cm_c(:) = drop_radius_um(:, k) *  1.e-4_r8 ! 10.0_r8  *  1.e-6_r8 ! fkm for Cloud reactions; from MG - liquid cloud drop radius in [um] * 1e4 -> cm
       
       radius_cm_a1(:) = 0.5_r8*dgncur_awet(1:ncol,k,1) * 1.e-2_r8   ! fkm for Phase Transfer; radius of wet aerosol = geometric mean wet diameter for number distribution (m -> cm)
       radius_cm_a2(:) = 0.5_r8*dgncur_awet(1:ncol,k,2) * 1.e-2_r8   ! fkm for Phase Transfer; radius of wet aerosol = geometric mean wet diameter for number distribution (m -> cm)
       radius_cm_a3(:) = 0.5_r8*dgncur_awet(1:ncol,k,3) * 1.e-2_r8   ! fkm for Phase Transfer; radius of wet aerosol = geometric mean wet diameter for number distribution (m -> cm)
       
       ! fkm for MOSAIC pH
       pH_a1(:) = min( 14._r8, max(0._r8, pH_buf(1:ncol,k,1) ) )
       pH_a2(:) = min( 14._r8, max(0._r8, pH_buf(1:ncol,k,2) ) )
       pH_a3(:) = min( 14._r8, max(0._r8, pH_buf(1:ncol,k,3) ) )
       
       ! fkm for Phaes Transfer; converting the rates of n-th order reactions from M^(1-n) s^-1 to (molecules cm^-3)^(1-n) s^-1
       conv_aq_rate_coeff_a1(:) = 1.e3_r8 / (avo * alwc_vol_vol_a1(:)) 
       conv_aq_rate_coeff_a2(:) = 1.e3_r8 / (avo * alwc_vol_vol_a2(:))
       conv_aq_rate_coeff_a3(:) = 1.e3_r8 / (avo * alwc_vol_vol_a3(:))

       ! ----------------------------------------
       ! WSY for Phase Transfer: phase-transfer on wet aerosols: SO2
       ! ----------------------------------------
       if ( g2a_SO2_a1_ndx>0 .or. a2g_SO2_a1_ndx>0 .or. &
         g2a_SO2_a2_ndx>0 .or. a2g_SO2_a2_ndx>0 .or. &
         g2a_SO2_a3_ndx>0 .or. a2g_SO2_a3_ndx>0 ) then
         
         id_gas   = get_spc_ndx( 'SO2' )
         id_aq_a1 = get_spc_ndx( 'SIV_a1' )
         id_aq_a2 = get_spc_ndx( 'SIV_a2' )
         id_aq_a3 = get_spc_ndx( 'SIV_a3' )
         MW_gas_kg_mol = 0.064066_r8
         mass_accommodation_coeff = 0.035_r8
         thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
         
         do i = 1,ncol
           
           heff_M_atm_a1(i) = heff_acid_lookup("SO2             ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
           heff_M_atm_a2(i) = heff_acid_lookup("SO2             ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
           heff_M_atm_a3(i) = heff_acid_lookup("SO2             ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
         enddo
         
         rxt(:,k,g2a_SO2_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
         rxt(:,k,a2g_SO2_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp(:,k) / R_atm_per_K_per_M
         rxt(:,k,g2a_SO2_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
         rxt(:,k,a2g_SO2_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp(:,k) / R_atm_per_K_per_M
         rxt(:,k,g2a_SO2_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
         rxt(:,k,a2g_SO2_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp(:,k) / R_atm_per_K_per_M
         
       end if
       
     ! ----------------------------------------
     ! WSY for Phase Transfer: phase-transfer on wet aerosols: H2O2
     ! ----------------------------------------
     if ( g2a_H2O2_a1_ndx>0 .or. a2g_H2O2_a1_ndx>0 .or. &
       g2a_H2O2_a2_ndx>0 .or. a2g_H2O2_a2_ndx>0 .or. &
       g2a_H2O2_a3_ndx>0 .or. a2g_H2O2_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'H2O2' )
       id_aq_a1 = get_spc_ndx( 'H2O2_a1' )
       id_aq_a2 = get_spc_ndx( 'H2O2_a2' )
       id_aq_a3 = get_spc_ndx( 'H2O2_a3' )
       MW_gas_kg_mol = 0.0340147_r8
       mass_accommodation_coeff = 0.018_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("H2O2            ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("H2O2            ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("H2O2            ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       rxt(:,k,g2a_H2O2_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:) * max( 0._r8, 1._r8 - cldfrc(:,k) )
       rxt(:,k,a2g_H2O2_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:ncol,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_H2O2_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:) * max( 0._r8, 1._r8 - cldfrc(:,k) )
       rxt(:,k,a2g_H2O2_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:ncol,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_H2O2_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:) * max( 0._r8, 1._r8 - cldfrc(:,k) )
       rxt(:,k,a2g_H2O2_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:ncol,k) / R_atm_per_K_per_M
       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: OH
     ! ----------------------------------------
     if ( g2a_OH_a1_ndx>0 .or. a2g_OH_a1_ndx>0 .or. &
       g2a_OH_a2_ndx>0 .or. a2g_OH_a2_ndx>0 .or. &
       g2a_OH_a3_ndx>0 .or. a2g_OH_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'OH' )
       id_aq_a1 = get_spc_ndx( 'OH_a1' )
       id_aq_a2 = get_spc_ndx( 'OH_a2' )
       id_aq_a3 = get_spc_ndx( 'OH_a3' )
       MW_gas_kg_mol = 17.0067997e-3_r8
       mass_accommodation_coeff = 0.001_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("OH              ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("OH              ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("OH              ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       rxt(:,k,g2a_OH_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_OH_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:ncol,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_OH_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_OH_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:ncol,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_OH_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_OH_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:ncol,k) / R_atm_per_K_per_M
       
       kt_OH_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_OH_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_OH_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))
       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: O3
     ! ----------------------------------------
     if ( g2a_O3_a1_ndx>0 .or. a2g_O3_a1_ndx>0 .or. &
       g2a_O3_a2_ndx>0 .or. a2g_O3_a2_ndx>0 .or. &
       g2a_O3_a3_ndx>0 .or. a2g_O3_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'O3' )
       id_aq_a1 = get_spc_ndx( 'O3_a1' )
       id_aq_a2 = get_spc_ndx( 'O3_a2' )
       id_aq_a3 = get_spc_ndx( 'O3_a3' )
       MW_gas_kg_mol = 47.9981995e-3_r8
       mass_accommodation_coeff = 5.3e-4_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("OX              ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("OX              ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("OX              ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       ! write(iulog,*) 'before calculating rxt for g2a, a2g O3_a'
       rxt(:,k,g2a_O3_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:) 
       rxt(:,k,a2g_O3_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_O3_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_O3_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_O3_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_O3_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M
       
       kt_O3_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_O3_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_O3_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))
       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: DMS
     ! ----------------------------------------
     if ( g2a_DMS_a1_ndx>0 .or. a2g_DMS_a1_ndx>0 .or. &
       g2a_DMS_a2_ndx>0 .or. a2g_DMS_a2_ndx>0 .or. &
       g2a_DMS_a3_ndx>0 .or. a2g_DMS_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'DMS' )
       id_aq_a1 = get_spc_ndx( 'DMS_a1' )
       id_aq_a2 = get_spc_ndx( 'DMS_a2' )
       id_aq_a3 = get_spc_ndx( 'DMS_a3' )
       MW_gas_kg_mol = 62.1324e-3_r8
       mass_accommodation_coeff = 0.05_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("DMS             ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("DMS             ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("DMS             ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo

       ! write(iulog,*) 'before calculating rxt for g2a, a2g DMS_a'
       rxt(:,k,g2a_DMS_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_DMS_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_DMS_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_DMS_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_DMS_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_DMS_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M
       
       kt_DMS_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_DMS_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_DMS_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))

       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: DMSO
     ! ----------------------------------------
     if ( g2a_DMSO_a1_ndx>0 .or. a2g_DMSO_a1_ndx>0 .or. &
       g2a_DMSO_a2_ndx>0 .or. a2g_DMSO_a2_ndx>0 .or. &
       g2a_DMSO_a3_ndx>0 .or. a2g_DMSO_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'DMSO' )
       id_aq_a1 = get_spc_ndx( 'DMSO_a1' )
       id_aq_a2 = get_spc_ndx( 'DMSO_a2' )
       id_aq_a3 = get_spc_ndx( 'DMSO_a3' )
       MW_gas_kg_mol = 78.13e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("DMSO            ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("DMSO            ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("DMSO            ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       ! write(iulog,*) 'before calculating rxt for g2a, a2g DMSO_a'
       rxt(:,k,g2a_DMSO_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_DMSO_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_DMSO_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_DMSO_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_DMSO_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_DMSO_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M
       
       kt_DMSO_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_DMSO_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_DMSO_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))
       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: MSIA
     ! ----------------------------------------
     if ( g2a_MSIA_a1_ndx>0 .or. a2g_MSIA_a1_ndx>0 .or. &
       g2a_MSIA_a2_ndx>0 .or. a2g_MSIA_a2_ndx>0 .or. &
       g2a_MSIA_a3_ndx>0 .or. a2g_MSIA_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'MSIA' )
       id_aq_a1 = get_spc_ndx( 'MSIA_a1' )
       id_aq_a2 = get_spc_ndx( 'MSIA_a2' )
       id_aq_a3 = get_spc_ndx( 'MSIA_a3' )
       MW_gas_kg_mol = 80.11e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("MSIA            ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("MSIA            ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("MSIA            ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       ! write(iulog,*) 'before calculating rxt for g2a, a2g MSIA_a'
       rxt(:,k,g2a_MSIA_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_MSIA_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_MSIA_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_MSIA_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M 
       rxt(:,k,g2a_MSIA_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_MSIA_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M
       
       kt_MSIA_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_MSIA_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_MSIA_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))
              
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: MSA
     ! ----------------------------------------
     if ( g2a_MSA_a1_ndx>0 .or. a2g_MSA_a1_ndx>0 .or. &
       g2a_MSA_a2_ndx>0 .or. a2g_MSA_a2_ndx>0 .or. &
       g2a_MSA_a3_ndx>0 .or. a2g_MSA_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'MSA' )
       id_aq_a1 = get_spc_ndx( 'MSA_a1' )
       id_aq_a2 = get_spc_ndx( 'MSA_a2' )
       id_aq_a3 = get_spc_ndx( 'MSA_a3' )
       MW_gas_kg_mol = 96.10e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("MSA             ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("MSA             ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("MSA             ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       ! write(iulog,*) 'before calculating rxt for g2a, a2g MSA_a'
       rxt(:,k,g2a_MSA_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_MSA_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_MSA_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_MSA_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_MSA_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_MSA_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M
       
       kt_MSA_a1(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:))
       kt_MSA_a2(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:))
       kt_MSA_a3(:) = max( 0._r8, kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:))
       
     end if
     
     ! ----------------------------------------
     ! fkm for Phase Transfer: phase-transfer on wet aerosols: HOBR
     ! ----------------------------------------
     if ( g2a_HOBR_a1_ndx>0 .or. a2g_HOBR_a1_ndx>0 .or. &
       g2a_HOBR_a2_ndx>0 .or. a2g_HOBR_a2_ndx>0 .or. &
       g2a_HOBR_a3_ndx>0 .or. a2g_HOBR_a3_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'HOBR' )
       id_aq_a1 = get_spc_ndx( 'HOBR_a1' )
       id_aq_a2 = get_spc_ndx( 'HOBR_a2' )
       id_aq_a3 = get_spc_ndx( 'HOBR_a3' )
       MW_gas_kg_mol = 96.91e-3_r8
       mass_accommodation_coeff = 0.6_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_a1(i) = heff_acid_lookup("HOBR            ", temp (i,k), pH_a1(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a2(i) = heff_acid_lookup("HOBR            ", temp (i,k), pH_a2(i), .true.)  ! make sure the lenth of the input string is 16
         heff_M_atm_a3(i) = heff_acid_lookup("HOBR            ", temp (i,k), pH_a3(i), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       ! write(iulog,*) 'before calculating rxt for g2a, a2g HOBR_a'
       rxt(:,k,g2a_HOBR_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a1(:)
       rxt(:,k,a2g_HOBR_a1_ndx) = kt_s_wetaerosols(ncol, radius_cm_a1(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a1(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_HOBR_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a2(:)
       rxt(:,k,a2g_HOBR_a2_ndx) = kt_s_wetaerosols(ncol, radius_cm_a2(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a2(:) / temp (:,k) / R_atm_per_K_per_M
       rxt(:,k,g2a_HOBR_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_a3(:)
       rxt(:,k,a2g_HOBR_a3_ndx) = kt_s_wetaerosols(ncol, radius_cm_a3(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_a3(:) / temp (:,k) / R_atm_per_K_per_M

     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: DMS
     ! ----------------------------------------
     if ( g2c_DMS_c_ndx>0 .or. c2g_DMS_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'DMS' )
       id_aq_c  = get_spc_ndx( 'DMS_c' )
      ! faq_DMS(:) = small_value
      
       MW_gas_kg_mol = 62.1324e-3_r8
       mass_accommodation_coeff = 0.05_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("DMS             ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_DMS(:) = heff_M_atm_c(:)
       
       faq_DMS(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_DMS(:) = max( 0._r8, faq_DMS(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_DMS_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_DMS_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M
     
     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: DMSO
     ! ----------------------------------------
     if ( g2c_DMSO_c_ndx>0 .or. c2g_DMSO_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'DMSO' )
       id_aq_c  = get_spc_ndx( 'DMSO_c' )

       MW_gas_kg_mol = 78.13e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)

       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("DMSO            ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_DMSO(:) = heff_M_atm_c(:)
       
       faq_DMSO(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_DMSO(:) = max( 0._r8, faq_DMSO(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_DMSO_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_DMSO_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M
       
     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: MSIA
     ! ----------------------------------------
     if ( g2c_MSIA_c_ndx>0 .or. c2g_MSIA_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'MSIA' )
       id_aq_c  = get_spc_ndx( 'MSIA_c' )

       MW_gas_kg_mol = 80.11e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
              
       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("MSIA            ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_MSIA(:) = heff_M_atm_c(:)
       
       faq_MSIA(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_MSIA(:) = max( 0._r8, faq_MSIA(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_MSIA_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_MSIA_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M
       
     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: MSA
     ! ----------------------------------------
     if ( g2c_MSA_c_ndx>0 .or. c2g_MSA_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'MSA' )
       id_aq_c  = get_spc_ndx( 'MSA_c' )
       
       MW_gas_kg_mol = 96.10e-3_r8
       mass_accommodation_coeff = 0.1_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)

       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("MSA             ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_MSA(:) = heff_M_atm_c(:)
       
       faq_MSA(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_MSA(:) = max( 0._r8, faq_MSA(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_MSA_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_MSA_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M

     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: OH
     ! ----------------------------------------
     if ( g2c_OH_c_ndx>0 .or. c2g_OH_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'OH' )
       id_aq_c  = get_spc_ndx( 'OH_c' )
       
       MW_gas_kg_mol = 17.0067997e-3_r8
       mass_accommodation_coeff = 0.001_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("OH              ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_OH(:) = heff_M_atm_c(:)
       
       faq_OH(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_OH(:) = max( 0._r8, faq_OH(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_OH_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_OH_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M
       
     end if
     
     ! ----------------------------------------
     ! fkm for Cloud Reactions: phase-transfer to cloud-borne: O3
     ! ----------------------------------------
     if ( g2c_O3_c_ndx>0 .or. c2g_O3_c_ndx>0 ) then
       
       id_gas   = get_spc_ndx( 'O3' )
       id_aq_c  = get_spc_ndx( 'O3_c' )
       
       MW_gas_kg_mol = 47.9981995e-3_r8
       mass_accommodation_coeff = 5.3e-4_r8
       thermalspeed_cm_s(:) = 100.0_r8 * sqrt(8.0_r8 * 8.314_r8 * temp (:ncol,k) / 3.14159_r8 / MW_gas_kg_mol)
       
       do i = 1,ncol
         heff_M_atm_c(i) = heff_acid_lookup("OX              ", temp (i,k), cloud_ph(i,k), .true.)  ! make sure the lenth of the input string is 16
       enddo
       
       he_O3(:) = heff_M_atm_c(:)
       
       faq_O3(:) = heff_M_atm_c(:) * lwc(:) * temp (:,k) * R_atm_per_K_per_M   ! fkm: f_aq = distribution factor of aq/g from Seinfeld Eq (7.6)
       
       faq_O3(:) = max( 0._r8, faq_O3(:) ) ! fkm: to ensure faq is non-zero
       
       rxt(:,k,g2c_O3_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) * alwc_vol_vol_c(:)
       rxt(:,k,c2g_O3_c_ndx) = kt_s_wetaerosols(ncol, radius_cm_c(:), dg, thermalspeed_cm_s(:), mass_accommodation_coeff) / heff_M_atm_c(:) / temp (:,k) / R_atm_per_K_per_M

     end if
     
     ! ===================================================================================
     ! FKM for Cloud Reactions
     ! DMS + O3 -> DMSO
     ! ===================================================================================
     if ( DMS_O3_c_ndx>0 ) then
       
       do i = 1,ncol

        rxt(i,k,DMS_O3_c_ndx) = k_DMS_O3_M_s(cloud_ph(i,k), temp (i,k))  &
                                 * conv_aq_rate_coeff_c(i)
       end do
       
     end if
     
     ! ===================================================================================
     ! FKM for Cloud Reactions
     ! DMSO + OH -> MSIA
     ! ===================================================================================
     if ( DMSO_OH_c_ndx>0 ) then
     
       do i = 1,ncol
         rxt(i,k,DMSO_OH_c_ndx) = k_DMSO_OH_M_s(cloud_ph(i,k), temp (i,k))  &
                                  * conv_aq_rate_coeff_c(i)
       end do
     
     end if
     
     ! ===================================================================================
     ! FKM for Cloud Reactions
     ! MSIA + OH -> MSA
     ! MSI- + OH -> MSA
     ! ===================================================================================
     if ( MSIA_OH_c_ndx>0 ) then
     
       do i = 1,ncol

          rxt(i,k,MSIA_OH_c_ndx) = k_MSIA_OH_M_s(cloud_ph(i,k), temp (i,k))  &
                                   * conv_aq_rate_coeff_c(i)
                                  
       end do
     
     end if
     
     ! ===================================================================================
     ! FKM for Cloud Reactions
     ! MSIA + O3 -> MSA
     ! MSI- + O3 -> MS-
     ! ===================================================================================
     if ( MSIA_O3_c_ndx>0 ) then
     
       do i = 1,ncol
         
         rxt(i,k,MSIA_O3_c_ndx) = k_MSIA_O3_M_s(cloud_ph(i,k), temp (i,k))  &
                                  * conv_aq_rate_coeff_c(i) 
       end do
     
     end if
     
     ! ===================================================================================
     ! FKM for Cloud Reactions
     ! MSIA + O3 -> MSA
     ! MSI- + O3 -> MS-
     ! ===================================================================================
     if ( MSA_OH_c_ndx>0 ) then
     
       do i = 1,ncol
         
         rxt(i,k,MSA_OH_c_ndx) = k_MSA_OH_M_s(cloud_ph(i,k), temp (i,k))  &
                                  * conv_aq_rate_coeff_c(i) 
       end do
     
     end if
   
   ! ==================================================================================
   ! WSY for Phase Transfer: aqueous-phase reactions on wet aerosols: H2O2
   ! Siyuan Wang (siyuan@ucar.edu)
   ! NOTE: Unit conversion may be needed!!!
   !       For most aqueous-phase reaction, the rate coefficients are in M^(1-n) s^-1
   !       where n is the reaction order.
   !       But when solving the ODEs, the units should all be in (molec/cm^3)^(1-n) s^-1
   !       That is, if the reaction is 1st order, e.g., H2O2(aq) + hv = 2 OH(aq), 
   !       no unit conversion is needed. 
   !       For n-th order reactions, this conversion factor is needed!!! e.g.,
   !       2nd reaction:                      1/conv_aq_rate_coeff_a1
   !       3rd reaction:                      1/conv_aq_rate_coeff_a1/conv_aq_rate_coeff_a1
   ! ===================================================================================

   if ( alwc_SIV_H2O2_a1_ndx>0 .or. alwc_SIV_H2O2_a2_ndx>0 .or. alwc_SIV_H2O2_a3_ndx>0 ) then

     do i = 1,ncol
       rxt(i,k,alwc_SIV_H2O2_a1_ndx) = k_H2O2_SO2_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,alwc_SIV_H2O2_a2_ndx) = k_H2O2_SO2_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,alwc_SIV_H2O2_a3_ndx) = k_H2O2_SO2_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! DMS + O3 -> DMSO
   ! ===================================================================================
   if ( DMS_O3_a1_ndx>0 .or. DMS_O3_a2_ndx>0 .or. DMS_O3_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,DMS_O3_a1_ndx) = k_DMS_O3_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,DMS_O3_a2_ndx) = k_DMS_O3_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,DMS_O3_a3_ndx) = k_DMS_O3_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! DMSO + OH -> MSIA
   ! ===================================================================================
   if ( DMSO_OH_a1_ndx>0 .or. DMSO_OH_a2_ndx>0 .or. DMSO_OH_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,DMSO_OH_a1_ndx) = k_DMSO_OH_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,DMSO_OH_a2_ndx) = k_DMSO_OH_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,DMSO_OH_a3_ndx) = k_DMSO_OH_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! MSIA + OH -> MSA
   ! MSI- + OH -> MSA
   ! ===================================================================================
   if ( MSIA_OH_a1_ndx>0 .or. MSIA_OH_a2_ndx>0 .or. MSIA_OH_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,MSIA_OH_a1_ndx) = k_MSIA_OH_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,MSIA_OH_a2_ndx) = k_MSIA_OH_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,MSIA_OH_a3_ndx) = k_MSIA_OH_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! MSIA + O3 -> MSA
   ! MSI- + O3 -> MS-
   ! ===================================================================================
   if ( MSIA_O3_a1_ndx>0 .or. MSIA_O3_a2_ndx>0 .or. MSIA_O3_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,MSIA_O3_a1_ndx) = k_MSIA_O3_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,MSIA_O3_a2_ndx) = k_MSIA_O3_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,MSIA_O3_a3_ndx) = k_MSIA_O3_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! MSIA + O3 -> MSA
   ! MSI- + O3 -> MS-
   ! ===================================================================================
   if ( MSA_OH_a1_ndx>0 .or. MSA_OH_a2_ndx>0 .or. MSA_OH_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,MSA_OH_a1_ndx) = k_MSA_OH_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,MSA_OH_a2_ndx) = k_MSA_OH_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,MSA_OH_a3_ndx) = k_MSA_OH_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! HSO3- + HOBR -> SO4 + 2H + Br
   ! SO3-- + HOBR -> SO4 + 2H + Br
   ! ===================================================================================
   if ( SIV_HOBR_a1_ndx>0 .or. SIV_HOBR_a2_ndx>0 .or. SIV_HOBR_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,SIV_HOBR_a1_ndx) = k_SIV_HOBR_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,SIV_HOBR_a2_ndx) = k_SIV_HOBR_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,SIV_HOBR_a3_ndx) = k_SIV_HOBR_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if
   
   ! ===================================================================================
   ! FKM for Phase transfer: aqueous-phase reactions on wet aerosols:
   ! HSO3- + O3 -> SO4 + O2 + H+
   ! SO3-- + O3 -> SO4 + O2
   ! ===================================================================================
   if ( SIV_O3_a1_ndx>0 .or. SIV_O3_a2_ndx>0 .or. SIV_O3_a3_ndx>0 ) then
     
     do i = 1,ncol
       rxt(i,k,SIV_O3_a1_ndx) = k_SIV_O3_M_s(pH_a1(i), temp (i,k)) * conv_aq_rate_coeff_a1(i)
       rxt(i,k,SIV_O3_a2_ndx) = k_SIV_O3_M_s(pH_a2(i), temp (i,k)) * conv_aq_rate_coeff_a2(i)
       rxt(i,k,SIV_O3_a3_ndx) = k_SIV_O3_M_s(pH_a3(i), temp (i,k)) * conv_aq_rate_coeff_a3(i)
     end do
     
   end if

!-----------------------------------------------------------------
!	... o + o2 + m --> o3 + m (JPL15-10)
!-----------------------------------------------------------------
       if( usr_O_O2_ndx > 0 ) then
          rxt(:,k,usr_O_O2_ndx) = 6.e-34_r8 * tp(:)**2.4_r8
       end if
       if( usr_OA_O2_ndx > 0 ) then
          rxt(:,k,usr_OA_O2_ndx) = 6.e-34_r8 * tp(:)**2.4_r8
       end if

!-----------------------------------------------------------------
!	... o + o + m -> o2 + m
!-----------------------------------------------------------------
       if ( usr_O_O_ndx > 0 ) then
          rxt(:,k,usr_O_O_ndx) = 2.76e-34_r8 * exp( 720.0_r8*tinv(:) )
       end if

!-----------------------------------------------------------------
! 	... cl2o2 + m -> 2*clo + m  (JPL15-10)
!-----------------------------------------------------------------
       if ( usr_CL2O2_M_ndx > 0 ) then
          if ( tag_CLO_CLO_M_ndx > 0 ) then
             ko(:)            = 2.16e-27_r8 * exp( 8537.0_r8* tinv(:) )
             rxt(:,k,usr_CL2O2_M_ndx) = rxt(:,k,tag_CLO_CLO_M_ndx)/ko(:)
          else
             rxt(:,k,usr_CL2O2_M_ndx) = 0._r8
          end if
       end if

!-----------------------------------------------------------------
!       ... so3 + 2*h2o --> h2so4 + h2o
!       Note: this reaction proceeds by the 2 intermediate steps below
!           so3 + h2o --> adduct
!           adduct + h2o --> h2so4 + h2o
!               (Lovejoy et al., JCP, pp. 19911-19916, 1996)
!	The first order rate constant used here is recommended by JPL 2011.
!	This rate involves the water vapor number density.
!-----------------------------------------------------------------

       if ( usr_SO3_H2O_ndx > 0 ) then
          call comp_exp( exp_fac, 6540.0_r8*tinv(:), ncol )
          if( h2o_ndx > 0 ) then
             fc(:) = 8.5e-21_r8 * m(:,k) * h2ovmr(:,k) * exp_fac(:)
          else
             fc(:) = 8.5e-21_r8 * invariants(:,k,inv_h2o_ndx) * exp_fac(:)
          end if
          rxt(:,k,usr_SO3_H2O_ndx) = 1.0e-20_r8 * fc(:)
       end if

!-----------------------------------------------------------------
!	... n2o5 + m --> no2 + no3 + m (JPL15-10)
!-----------------------------------------------------------------
       if( usr_N2O5_M_ndx > 0 ) then
          if( tag_NO2_NO3_ndx > 0 ) then
             call comp_exp( exp_fac, -10840.0_r8*tinv, ncol )
             rxt(:,k,usr_N2O5_M_ndx) = rxt(:,k,tag_NO2_NO3_ndx) * 1.724138e26_r8 * exp_fac(:)
          else
             rxt(:,k,usr_N2O5_M_ndx) = 0._r8
          end if
       end if
       if( usr_XNO2NO3_M_ndx > 0 ) then
          if( tag_NO2_NO3_ndx > 0 ) then
             call comp_exp( exp_fac, -10840.0_r8*tinv, ncol )
             rxt(:,k,usr_XNO2NO3_M_ndx) = rxt(:,k,tag_NO2_NO3_ndx) *1.724138e26_r8 * exp_fac(:)
          else
             rxt(:,k,usr_XNO2NO3_M_ndx) = 0._r8
          end if
       end if
       if( usr_NO2XNO3_M_ndx > 0 ) then
          if( tag_NO2_NO3_ndx > 0 ) then
             call comp_exp( exp_fac, -10840.0_r8*tinv, ncol )
             rxt(:,k,usr_NO2XNO3_M_ndx) = rxt(:,k,tag_NO2_NO3_ndx) * 1.734138e26_r8 * exp_fac(:)
          else
             rxt(:,k,usr_NO2XNO3_M_ndx) = 0._r8
          end if
       end if

!-----------------------------------------------------------------
!	set rates for:
! 	... hno3 + oh --> no3 + h2o
!           ho2no2 + m --> ho2 + no2 + m
!-----------------------------------------------------------------
       if( usr_HNO3_OH_ndx > 0 ) then
          call comp_exp( exp_fac, 1335._r8*tinv, ncol )
          ko(:) = m(:,k) * 6.5e-34_r8 * exp_fac(:)
          call comp_exp( exp_fac, 2199._r8*tinv, ncol )
          ko(:) = ko(:) / (1._r8 + ko(:)/(2.7e-17_r8*exp_fac(:)))
          call comp_exp( exp_fac, 460._r8*tinv, ncol )
          rxt(:,k,usr_HNO3_OH_ndx) = ko(:) + 2.4e-14_r8*exp_fac(:)
       end if
       if( usr_XHNO3_OH_ndx > 0 ) then
          call comp_exp( exp_fac, 1335._r8*tinv, ncol )
          ko(:) = m(:,k) * 6.5e-34_r8 * exp_fac(:)
          call comp_exp( exp_fac, 2199._r8*tinv, ncol )
          ko(:) = ko(:) / (1._r8 + ko(:)/(2.7e-17_r8*exp_fac(:)))
          call comp_exp( exp_fac, 460._r8*tinv, ncol )
          rxt(:,k,usr_XHNO3_OH_ndx) = ko(:) + 2.4e-14_r8*exp_fac(:)
       end if
       if( usr_HO2NO2_M_ndx > 0 ) then
          if( tag_NO2_HO2_ndx > 0 ) then
             call comp_exp( exp_fac, -10900._r8*tinv, ncol )
             rxt(:,k,usr_HO2NO2_M_ndx) = rxt(:,k,tag_NO2_HO2_ndx) * exp_fac(:) / 2.1e-27_r8
          else
             rxt(:,k,usr_HO2NO2_M_ndx) = 0._r8
          end if
       end if
       if( usr_XHO2NO2_M_ndx > 0 ) then
          if( tag_NO2_HO2_ndx > 0 ) then
             call comp_exp( exp_fac, -10900._r8*tinv, ncol )
             rxt(:,k,usr_XHO2NO2_M_ndx) = rxt(:,k,tag_NO2_HO2_ndx) * exp_fac(:) / 2.1e-27_r8
          else
             rxt(:,k,usr_XHO2NO2_M_ndx) = 0._r8
          end if
       end if
!-----------------------------------------------------------------
!           co + oh --> co2 + ho2     (combined branches - do not use with CO_OH_b)
!-----------------------------------------------------------------
       if( usr_CO_OH_a_ndx > 0 ) then
          rxt(:,k,usr_CO_OH_a_ndx) = 1.5e-13_r8 * &
               (1._r8 + 6.e-7_r8*boltz_cgs*m(:,k)*temp(:ncol,k))
       end if
!-----------------------------------------------------------------
! 	... co + oh --> co2 + h (second branch JPL15-10, with CO+OH+M)
!-----------------------------------------------------------------
       if( usr_CO_OH_b_ndx > 0 ) then
         kinf(:)  = 2.1e+09_r8 * (temp(:ncol,k)/ t0)**(6.1_r8)
         ko  (:)  = 1.5e-13_r8

         term1(:) = ko(:) / ( (kinf(:) / m(:,k)) )
         term2(:) = ko(:) / (1._r8 + term1(:))

         term1(:) = log10( term1(:) )
         term1(:) = 1.0_r8 / (1.0_r8 + term1(:)*term1(:))

         rxt(:ncol,k,usr_CO_OH_b_ndx) = term2(:) * (0.6_r8)**term1(:)
       end if

!-----------------------------------------------------------------
!       ... ho2 + ho2 --> h2o2
!       note: this rate involves the water vapor number density
!-----------------------------------------------------------------
       if( usr_HO2_HO2_ndx > 0 ) then

          call comp_exp( exp_fac, 460._r8*tinv, ncol )
          ko(:)   = 3.0e-13_r8 * exp_fac(:)
          call comp_exp( exp_fac, 920._r8*tinv, ncol )
          kinf(:) = 2.1e-33_r8 * m(:,k) * exp_fac(:)
          call comp_exp( exp_fac, 2200._r8*tinv, ncol )

          if( h2o_ndx > 0 ) then
             fc(:) = 1._r8 + 1.4e-21_r8 * m(:,k) * h2ovmr(:,k) * exp_fac(:)
          else
             fc(:) = 1._r8 + 1.4e-21_r8 * invariants(:,k,inv_h2o_ndx) * exp_fac(:)
          end if
          rxt(:,k,usr_HO2_HO2_ndx) = (ko(:) + kinf(:)) * fc(:)

       end if

!-----------------------------------------------------------------
!    	... mco3 + no2 -> mpan
!-----------------------------------------------------------------
       if( usr_MCO3_NO2_ndx > 0 ) then
          rxt(:,k,usr_MCO3_NO2_ndx) = 1.1e-11_r8 * tp(:) / m(:,k)
       end if
       if( usr_MCO3_XNO2_ndx > 0 ) then
          rxt(:,k,usr_MCO3_XNO2_ndx) = 1.1e-11_r8 * tp(:) / m(:,k)
       end if

!-----------------------------------------------------------------
!	... pan + m --> ch3co3 + no2 + m (JPL15-10)
!-----------------------------------------------------------------
       call comp_exp( exp_fac, -14000._r8*tinv, ncol )
       if( usr_PAN_M_ndx > 0 ) then
          if( tag_CH3CO3_NO2_ndx > 0 ) then
             rxt(:,k,usr_PAN_M_ndx) = rxt(:,k,tag_CH3CO3_NO2_ndx) * 1.111e28_r8 * exp_fac(:)
          else
             rxt(:,k,usr_PAN_M_ndx) = 0._r8
          end if
       end if
       if( usr_XPAN_M_ndx > 0 ) then
          if( tag_CH3CO3_NO2_ndx > 0 ) then
             rxt(:,k,usr_XPAN_M_ndx) = rxt(:,k,tag_CH3CO3_NO2_ndx) * 1.111e28_r8 * exp_fac(:)
          else
             rxt(:,k,usr_XPAN_M_ndx) = 0._r8
          end if
       end if

!-----------------------------------------------------------------
!	... mpan + m --> mco3 + no2 + m (JPL15-10)
!-----------------------------------------------------------------
       if( usr_MPAN_M_ndx > 0 ) then
          if( usr_MCO3_NO2_ndx > 0 ) then
             rxt(:,k,usr_MPAN_M_ndx) = rxt(:,k,usr_MCO3_NO2_ndx) * 1.111e28_r8 * exp_fac(:)
          else
             rxt(:,k,usr_MPAN_M_ndx) = 0._r8
          end if
       end if
       if( usr_XMPAN_M_ndx > 0 ) then
          if( usr_MCO3_NO2_ndx > 0 ) then
             rxt(:,k,usr_XMPAN_M_ndx) = rxt(:,k,usr_MCO3_NO2_ndx) * 1.111e28_r8 * exp_fac(:)
          else
             rxt(:,k,usr_XMPAN_M_ndx) = 0._r8
          end if
       end if

!lke-TS1
!-----------------------------------------------------------------
!       ... pbznit + m --> acbzo2 + no2 + m
!-----------------------------------------------------------------
       if( usr_PBZNIT_M_ndx > 0 ) then
          if( tag_ACBZO2_NO2_ndx > 0 ) then
             rxt(:,k,usr_PBZNIT_M_ndx) = rxt(:,k,tag_ACBZO2_NO2_ndx) * 1.111e28_r8 * exp_fac(:)
          else
             rxt(:,k,usr_PBZNIT_M_ndx) = 0._r8
          end if
       end if

!-----------------------------------------------------------------
!       ... xooh + oh -> h2o + oh
!-----------------------------------------------------------------
       if( usr_XOOH_OH_ndx > 0 ) then
          call comp_exp( exp_fac, 253._r8*tinv, ncol )
          rxt(:,k,usr_XOOH_OH_ndx) = temp(:ncol,k)**2._r8 * 7.69e-17_r8 * exp_fac(:)
       end if

!-----------------------------------------------------------------
!       ... ch3coch3 + oh -> ro2 + h2o
!-----------------------------------------------------------------
       if( usr_CH3COCH3_OH_ndx > 0 ) then
          call comp_exp( exp_fac, -2000._r8*tinv, ncol )
          rxt(:,k,usr_CH3COCH3_OH_ndx) = 3.82e-11_r8 * exp_fac(:) + 1.33e-13_r8
       end if

!-----------------------------------------------------------------
!       ... DMS + OH  --> .5 * SO2
!-----------------------------------------------------------------
       if( usr_DMS_OH_ndx > 0 ) then
          call comp_exp( exp_fac, 7460._r8*tinv, ncol )
          ko(:) = 1._r8 + 5.5e-31_r8 * exp_fac * m(:,k) * 0.21_r8
          call comp_exp( exp_fac, 7810._r8*tinv, ncol )
          rxt(:,k,usr_DMS_OH_ndx) = 1.7e-42_r8 * exp_fac * m(:,k) * 0.21_r8 / ko(:)
       end if

!-----------------------------------------------------------------
!       ... DMS + OH  --> .5 * SO2 + .4 * DMSO + CH3O2    ! added by fkm for DMS intermediates
!-----------------------------------------------------------------
      if( usr_DMS_OHb_ndx > 0 ) then
          o2(:ncol) = invariants(:ncol,k,inv_o2_ndx)
          call comp_exp( exp_fac, 3644._r8*tinv, ncol )
          ko(:) = 1._r8 + 1.05e-5_r8 * o2(:) / m(:,k) * exp_fac
          call comp_exp( exp_fac, 5376._r8*tinv, ncol )
          rxt(:,k,usr_DMS_OHb_ndx) = 8.2e-39_r8 * o2(:) * exp_fac / ko(:)
      end if
      
      
!-----------------------------------------------------------------
!       ... MSP   -->  •OOCH2SCH2OOH    ! added by fkm for iso pathway
!-----------------------------------------------------------------
      if( usr_is_shift_1_ndx > 0 ) then
          rxt(:,k,usr_is_shift_1_ndx) = 2.24e11_r8 * exp(-9.8e3_r8 * tinv) * exp(1.03e8_r8 * tinv**3._r8)  ! fkm: k_iso = 0.04 s–1 at 293 K
          ! rxt(:,k,usr_is_shift_1_ndx) = 6.77809e11_r8 * exp(-9.8e3_r8 * tinv) * exp(1.03e8_r8 * tinv**3._r8) ! fkm: k_iso = 0.14 s-1 at 295 K by Jesse 
      end if
      
!-----------------------------------------------------------------
!       ... •OOCH2SCH2OOH  --> HPMTF (HOOCH2SCHO) + OH    ! added by fkm for iso pathway
!-----------------------------------------------------------------
      if( usr_is_shift_2_ndx > 0 ) then
        rxt(:,k,usr_is_shift_2_ndx) = 6.09e11_r8 * exp(-9.5e3_r8 * tinv) * exp(1.10e8_r8 * tinv**3._r8)
      end if
      
!-----------------------------------------------------------------
!       ... HPMTF  +   cloud --> sink    ! added by fkm for HPMTF uptake
!-----------------------------------------------------------------
      if( usr_is_HPMTF_cloud_ndx > 0 ) then
        rxt(:,k,usr_is_HPMTF_cloud_ndx) = 5.0e-5_r8 * cldfrc(:, k)
      end if

!-----------------------------------------------------------------
!       ... SO2 + OH  --> SO4  (REFERENCE?? - not Liao)
!-----------------------------------------------------------------
       if( usr_SO2_OH_ndx > 0 ) then
          fc(:) = 3.0e-31_r8 *(300._r8*tinv(:))**3.3_r8
          ko(:) = fc(:)*m(:,k)/(1._r8 + fc(:)*m(:,k)/1.5e-12_r8)
          rxt(:,k,usr_SO2_OH_ndx) = ko(:)*.6_r8**(1._r8 + (log10(fc(:)*m(:,k)/1.5e-12_r8))**2._r8)**(-1._r8)
        end if


!
! reduced hydrocarbon scheme
!
       if ( usr_C2O3_NO2_ndx > 0 ) then
          ko(:)   = 2.6e-28_r8 * m(:,k)
          kinf(:) = 1.2e-11_r8
          rxt(:,k,usr_C2O3_NO2_ndx) = (ko/(1._r8+ko/kinf)) * 0.6_r8**(1._r8/(1._r8+(log10(ko/kinf))**2))
       end if
       if ( usr_C2O3_XNO2_ndx > 0 ) then
          ko(:)   = 2.6e-28_r8 * m(:,k)
          kinf(:) = 1.2e-11_r8
          rxt(:,k,usr_C2O3_XNO2_ndx) = (ko/(1._r8+ko/kinf)) * 0.6_r8**(1._r8/(1._r8+(log10(ko/kinf))**2))
       end if
       if ( usr_C2H4_OH_ndx > 0 ) then
          ko(:)   = 1.0e-28_r8 * m(:,k)
          kinf(:) = 8.8e-12_r8
          rxt(:,k,usr_C2H4_OH_ndx) = (ko/(1._r8+ko/kinf)) * 0.6_r8**(1._r8/(1._r8+(log(ko/kinf))**2))
       end if
       if ( usr_XO2N_HO2_ndx > 0 ) then
          rxt(:,k,usr_XO2N_HO2_ndx) = rxt(:,k,tag_XO2N_NO_ndx)*rxt(:,k,tag_XO2_HO2_ndx)/(rxt(:,k,tag_XO2_NO_ndx)+1.e-36_r8)
       end if

!
! hydrolysis reactions on wetted aerosols
!
       if( usr_NO2_aer_ndx > 0 .or. usr_NO3_aer_ndx > 0 .or. usr_N2O5_aer_ndx > 0 .or. usr_HO2_aer_ndx > 0 &
         .or. usr_GLYOXAL_aer_ndx > 0 ) then

          long_loop : do i = 1,ncol

             sfc    => sfc_array(i,k,:)
             dm_aer => dm_array(i,k,:)

             c_n2o5 = 1.40e3_r8 * sqrt_t(i)         ! mean molecular speed of n2o5
             c_no3  = 1.85e3_r8 * sqrt_t(i)         ! mean molecular speed of no3
             c_no2  = 2.15e3_r8 * sqrt_t(i)         ! mean molecular speed of no2
             c_ho2  = 2.53e3_r8 * sqrt_t(i)         ! mean molecular speed of ho2
             c_glyoxal = 1.455e4_r8 * sqrt_t_58(i)  ! mean molecular speed of ho2
             c_isopnita = 1.20e3_r8 * sqrt_t(i)         ! mean molecular speed of isopnita
             c_isopnitb = 1.20e3_r8 * sqrt_t(i)         ! mean molecular speed of isopnitb
             c_onitr    = 1.20e3_r8 * sqrt_t(i)         ! mean molecular speed of onitr
             c_honitr   = 1.26e3_r8 * sqrt_t(i)         ! mean molecular speed of honitr
             c_terpnit  = 0.992e3_r8 * sqrt_t(i)        ! mean molecular speed of terpnit
             c_nterpooh = 0.957e3_r8 * sqrt_t(i)        ! mean molecular speed of nterpooh
             c_nc4cho   = 1.21e3_r8 * sqrt_t(i)         ! mean molecular speed of nc4cho
             c_nc4ch2oh = 1.20e3_r8 * sqrt_t(i)         ! mean molecular speed of nc4ch2oh

             !-------------------------------------------------------------------------
             !  Heterogeneous reaction rates for uptake of a gas on an aerosol:
             !    rxt = sfc / ( (rad_aer/Dg_gas) + (4/(c_gas*gamma_gas)))
             !-------------------------------------------------------------------------
             !-------------------------------------------------------------------------
             ! 	... n2o5 -> 2 hno3  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_N2O5_aer_ndx > 0 ) then
                rxt(i,k,usr_N2O5_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_n2o5, gamma_n2o5 )
             end if
             if( usr_XNO2NO3_aer_ndx > 0 ) then
                rxt(i,k,usr_XNO2NO3_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_n2o5, gamma_n2o5 )
             end if
             if( usr_NO2XNO3_aer_ndx > 0 ) then
                rxt(i,k,usr_NO2XNO3_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_n2o5, gamma_n2o5 )
             end if
             !-------------------------------------------------------------------------
             ! 	... no3 -> hno3  (on sulfate, nh4no3, oc, soa)
             !-------------------------------------------------------------------------
             if( usr_NO3_aer_ndx > 0 ) then
                rxt(i,k,usr_NO3_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_no3, gamma_no3 )
             end if
             if( usr_XNO3_aer_ndx > 0 ) then
                rxt(i,k,usr_XNO3_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_no3, gamma_no3 )
             end if
             !-------------------------------------------------------------------------
             ! 	... no2 -> 0.5 * (ho+no+hno3)  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_NO2_aer_ndx > 0 ) then
                rxt(i,k,usr_NO2_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_no2, gamma_no2 )
             end if
             if( usr_XNO2_aer_ndx > 0 ) then
                rxt(i,k,usr_XNO2_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_no2, gamma_no2 )
             end if

             !-------------------------------------------------------------------------
             ! 	... ho2 -> 0.5 * h2o2  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_HO2_aer_ndx > 0 ) then
                rxt(i,k,usr_HO2_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_ho2, gamma_ho2 )
             end if
             !-------------------------------------------------------------------------
             !  ... glyoxal ->  soag1  (on sulfate, nh4no3, oc2, soa)
             ! first order uptake, Fuchs and Sutugin, 1971,  dCg = 1/4 * gamma * ! A * |v_mol| * Cg * dt
             !-------------------------------------------------------------------------
             if( usr_GLYOXAL_aer_ndx > 0 ) then
                rxt(i,k,usr_GLYOXAL_aer_ndx) = hetrxtrate_gly( sfc, c_glyoxal, gamma_glyoxal )
             end if
             !-------------------------------------------------------------------------
             ! 	... ISOPNITA -> HNO3  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_ISOPNITA_aer_ndx > 0 ) then
                rxt(i,k,usr_ISOPNITA_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_isopnita, gamma_isopnita )
             end if
             !-------------------------------------------------------------------------
             ! 	... ISOPNITB -> HNO3  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_ISOPNITB_aer_ndx > 0 ) then
                rxt(i,k,usr_ISOPNITB_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_isopnitb, gamma_isopnitb )
             end if
             !-------------------------------------------------------------------------
             ! 	...  ONITR -> HNO3 (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_ONITR_aer_ndx > 0 ) then
                rxt(i,k,usr_ONITR_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_onitr, gamma_onitr )
             end if
             !-------------------------------------------------------------------------
             ! 	... HONITR -> HNO3  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_HONITR_aer_ndx > 0 ) then
                rxt(i,k,usr_HONITR_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_honitr, gamma_honitr )
             end if
             !-------------------------------------------------------------------------
             ! 	... TERPNIT -> HNO3  (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_TERPNIT_aer_ndx > 0 ) then
                rxt(i,k,usr_TERPNIT_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_terpnit, gamma_terpnit )
             end if
             !-------------------------------------------------------------------------
             ! 	...  NTERPOOH -> HNO3 (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_NTERPOOH_aer_ndx > 0 ) then
                rxt(i,k,usr_NTERPOOH_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_nterpooh, gamma_nterpooh )
             end if
             !-------------------------------------------------------------------------
             ! 	...  NC4CHO -> HNO3 (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_NC4CHO_aer_ndx > 0 ) then
                rxt(i,k,usr_NC4CHO_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_nc4cho, gamma_nc4cho )
             end if
             !-------------------------------------------------------------------------
             ! 	...  NC4CH2OH -> HNO3 (on sulfate, nh4no3, oc2, soa)
             !-------------------------------------------------------------------------
             if( usr_NC4CH2OH_aer_ndx > 0 ) then
                rxt(i,k,usr_NC4CH2OH_aer_ndx) = hetrxtrate( sfc, dm_aer, dg, c_nc4ch2oh, gamma_nc4ch2oh )
             end if

          end do long_loop
       end if

       ! LLNL super fast chem reaction rates

       !-----------------------------------------------------------------------
       !     ... CO + OH --> CO2 + HO2
       !-----------------------------------------------------------------------
       if ( usr_oh_co_ndx > 0 ) then
          ko(:)     = 5.9e-33_r8 * tp(:)**1.4_r8
          kinf(:)   = 1.1e-12_r8 * (temp(:,k) / 300._r8)**1.3_r8
          ko_m(:)   = ko(:) * m(:,k)
          k0(:)     = 1.5e-13_r8 * (temp(:,k) / 300._r8)**0.6_r8
          kinf_m(:) = (2.1e+09_r8 * (temp(:,k) / 300._r8)**6.1_r8) / m(:,k)
          rxt(:,k,usr_oh_co_ndx) = (ko_m(:)/(1._r8+(ko_m(:)/kinf(:)))) * &
               0.6_r8**(1._r8/(1._r8+(log10(ko_m(:)/kinf(:)))**2._r8)) + &
               (k0(:)/(1._r8+(k0(:)/kinf_m(:)))) * &
               0.6_r8**(1._r8/(1._r8+(log10(k0(:)/kinf_m(:)))**2._r8))
       endif
       !-----------------------------------------------------------------------
       !     ... NO2 + H2O --> 0.5 HONO + 0.5 HNO3
       !-----------------------------------------------------------------------
       if ( het_no2_h2o_ndx > 0 ) then
          rxt(:,k,het_no2_h2o_ndx) = 4.0e-24_r8
       endif
       !-----------------------------------------------------------------------
       !     ... DMS + OH --> 0.75 SO2 + 0.25 MSA
       !-----------------------------------------------------------------------
       if ( usr_oh_dms_ndx > 0 ) then
          o2(:ncol) = invariants(:ncol,k,inv_o2_ndx)
          rxt(:,k,usr_oh_dms_ndx) = 2.000e-10_r8 * exp(5820.0_r8 * tinv(:)) / &
               ((2.000e29_r8 / o2(:)) + exp(6280.0_r8 * tinv(:)))
       endif
       
       if ( aq_so2_h2o2_ndx > 0 .or. aq_so2_o3_ndx > 0 ) then
          lwc(:) = cwat(:ncol,k) * invariants(:ncol,k,inv_m_ndx) * mbar(:ncol,k) /avo !PJC convert kg/kg to g/cm3
          !-----------------------------------------------------------------------
          !     ... SO2 + H2O2 --> S(VI)
          !-----------------------------------------------------------------------
          if ( aq_so2_h2o2_ndx > 0 ) then
             rxt(:,k,aq_so2_h2o2_ndx) = lwc(:) * 1.0e-03_r8 * avo * &
                  K_AQ * &

                  exp(ER_AQ * ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  HENRY298_SO2 * &
                  K298_SO2_HSO3 * &
                  HENRY298_H2O2 * &
                  exp(((H298_SO2 + H298_SO2_HSO3 + H298_H2O2) / R_CAL) * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  (R_CONC * temp(:ncol,k))**2.0e+00_r8 / &

                  (1.0e+00_r8 + 13.0e+00_r8 * 10.0e+00_r8**(-pH))
          endif
          
          !-----------------------------------------------------------------------
          !     ... SO2 + O3 --> S(VI)
          !-----------------------------------------------------------------------
          if (aq_so2_o3_ndx >0) then
             rxt(:,k,aq_so2_o3_ndx) = lwc(:) * 1.0e-03_r8 * avo * &
                  HENRY298_SO2 * exp((H298_SO2 / R_CAL) * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  (K0_AQ * exp(ER0_AQ * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) + &
                  K298_SO2_HSO3 * exp((H298_SO2_HSO3 / R_CAL) * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  (K1_AQ * exp(ER1_AQ * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) / &
                  10.0e+00_r8**(-pH) + K2_AQ * exp(ER2_AQ * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  K298_HSO3_SO3 * exp((H298_HSO3_SO3 / R_CAL) * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) / &
                  (10.0e+00_r8**(-pH))**2.0e+00_r8) ) * &
                  HENRY298_O3 * exp((H298_O3 / R_CAL) * &
                  ((1.0e+00_r8 / 298.0e+00_r8) - tinv(:))) * &
                  (R_CONC * temp(:ncol,k))**2.0e+00_r8
          endif

       endif

    if ( has_d_chem ) then

        call comp_exp( exp_fac, -600._r8 * tinv, ncol )
        rxt(:,k,ean_ndx(1))  = 1.e-31_r8 * tp(:) * exp_fac(:)
        rxt(:,k,ean_ndx(2))  = 9.1e-12_r8 * tp(:)**(-1.46_r8)
        call comp_exp( exp_fac, -193._r8 * tinv, ncol )
        rxt(:,k,ean_ndx(3))  = (4.e-30_r8 * exp_fac(:)) * 0.21_r8

        rxt(:,k,rpe_ndx(1))  = 4.2e-6_r8 * tp(:)**0.5_r8
        rxt(:,k,rpe_ndx(2))  = 6.3e-7_r8 * tp(:)**0.5_r8
        rxt(:,k,rpe_ndx(3))  = 2.5e-6_r8 * tp(:)**0.1_r8
        rxt(:,k,rpe_ndx(4))  = 2.48e-6_r8 * tp(:)**0.76_r8
        rxt(:,k,rpe_ndx(5))  = 1.4e-6_r8 * tp(:)**0.4_r8

        rxt(:,k,pir_ndx(1)) = 4.e-30_r8 * tp(:)**2.93_r8
        rxt(:,k,pir_ndx(2))  = 4.6e-27_r8 * tp(:)**4._r8

        call comp_exp( exp_fac, -15900._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(3))  = (2.5e-2_r8 * tp(:)**5._r8) * exp_fac(:)
        rxt(:,k,pir_ndx(4))  = 2.3e-27_r8 * tp(:)**7.5_r8

        call comp_exp( exp_fac, -10272._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(5))  = (2.6e-3_r8 * tp(:)**8.5_r8) * exp_fac(:)
        rxt(:,k,pir_ndx(6))  = 3.6e-27_r8 * tp(:)**8.1_r8

        call comp_exp( exp_fac, -9000._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(7))  = (1.5e-1_r8 * tp(:)**9.1_r8) * exp_fac(:)
        rxt(:,k,pir_ndx(8))  = 4.6e-28_r8 * tp(:)**14._r8

        call comp_exp( exp_fac, -6400._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(9))  = (1.7e-3_r8 * tp(:)**15._r8) * exp_fac(:)
        rxt(:,k,pir_ndx(10)) = 1.35e-28_r8 * tp(:)**2.83_r8

        rxt(:,k,pir_ndx(11)) = 1.e-27_r8 * (308._r8 * tinv(:))**4.7_r8
        rxt(:,k,pir_ndx(12)) = rxt(:,k,pir_ndx(11))
        rxt(:,k,pir_ndx(13)) = 1.4e-29_r8 * tp(:)**4._r8

        call comp_exp( exp_fac, -3872._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(14)) = (3.4e-7_r8 * tp(:)**5._r8) * exp_fac(:)

        rxt(:,k,pir_ndx(15)) = 3.0e-31_r8 * tp(:)**4.3_r8
        call comp_exp( exp_fac, -2093._r8 * tinv, ncol )
        rxt(:,k,pir_ndx(16)) = (1.5e-8_r8 * tp(:)**4.3_r8) * exp_fac(:)

        rxt(:,k,edn_ndx(1)) = 3.1e-10_r8 * tp(:)**0.83_r8
        call comp_exp( exp_fac, -4990._r8 * tinv, ncol )
        rxt(:,k,edn_ndx(2)) = (1.9e-12_r8 * tp(:)**(-1.5_r8)) * exp_fac(:)

        rxt(:,k,nir_ndx(1)) = 1.05e-12_r8 * tp(:)**2.15_r8
        rxt(:,k,nir_ndx(2)) = 2.5e-11_r8 * tp(:)**0.79_r8
        rxt(:,k,nir_ndx(3)) = 7.5e-11_r8 * tp(:)**0.79_r8
        rxt(:,k,nir_ndx(4)) = rxt(:,k,nir_ndx(1))
        rxt(:,k,nir_ndx(5)) = 1.3e-11_r8 * tp(:)**1.64_r8
        rxt(:,k,nir_ndx(6)) = 3.3e-11_r8 * tp(:)**2.38_r8

        call comp_exp( exp_fac, -7300_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(7)) = (1.0e-3_r8 * tp(:)) * exp_fac(:)
        call comp_exp( exp_fac, -7050_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(8)) = (7.2e-4_r8 * tp(:)) * exp_fac(:)
        call comp_exp( exp_fac, -6800_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(9)) = (6.5e-3_r8 * tp(:)) * exp_fac(:)
        call comp_exp( exp_fac, -7600_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(10)) = (5.7e-4_r8 * tp(:)) * exp_fac(:)

        call comp_exp( exp_fac, -7150_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(11)) = (1.5e-2_r8 * tp(:)) * exp_fac(:)

        call comp_exp( exp_fac, -13130_r8 * tinv, ncol )
        rxt(:,k,nir_ndx(12)) = (6.0e-3_r8 * tp(:)) * exp_fac(:)
        rxt(:,k,nir_ndx(13)) = 5.22e-28_r8 * tp(:)**2.62_r8

        rxt(:,k,iira_ndx(1)) = 6.0e-8_r8 * tp(:)**.5_r8
        do i = 2,niira
          rxt(:,k,iira_ndx(i)) = rxt(:,k,iira_ndx(i-1))
        enddo

        rxt(:,k,iirb_ndx(1)) = 1.25e-25_r8 * tp(:)**4._r8
        do i = 2,niirb
          rxt(:,k,iirb_ndx(i)) = rxt(:,k,iirb_ndx(i-1))
        enddo

        call comp_exp( exp_fac, -6600._r8 * tinv, ncol )
        rxt(:,k,usr_clm_h2o_m_ndx) = 2.e-8_r8 * exp_fac(:)

        call comp_exp( exp_fac, -11926._r8 * tinv, ncol )
        rxt(:,k,usr_clm_hcl_m_ndx) =  tinv(:) * exp_fac(:)

     endif
    end do level_loop

!-----------------------------------------------------------------
! 	... the ionic rates
!-----------------------------------------------------------------
    if ( has_ion_rxts ) then
       level_loop2 : do k = 1,pver
 	   tp(:ncol)         = (2._r8*tempi(:ncol,k) + temp(:ncol,k)) / ( 3._r8 * t0 )
	   tp(:)             = max( min( tp(:),20._r8 ),1._r8 )
	   rxt(:,k,ion1_ndx) = 2.82e-11_r8 + tp(:)*(-7.74e-12_r8 + tp(:)*(1.073e-12_r8  &
			 + tp(:)*(-5.17e-14_r8 + 9.65e-16_r8*tp(:))))
	   tp(:ncol)         = (.6363_r8*tempi(:ncol,k) + .3637_r8*temp(:ncol,k)) / t0
	   tp(:)             = max( min( tp(:),trlim2 ),1._r8 )
	   rxt(:,k,ion2_ndx) = 1.533e-12_r8 + tp(:)*(-5.92e-13_r8 + tp(:)*8.6e-14_r8)
	   tp(:ncol)         = 2._r8 * t0 /(tempi(:ncol,k) + temp(:ncol,k))
	   where( tp(:ncol) < trlim3 )
		  rxt(:,k,ion3_ndx)  = 1.4e-10_r8 * tp(:)**.44_r8
		  rxt(:,k,ion11_ndx) = 1.e-11_r8 * tp(:)**.23_r8
       elsewhere
		  rxt(:,k,ion3_ndx)  = 5.2e-11_r8 / tp(:)**.2_r8
	      rxt(:,k,ion11_ndx) = 3.6e-12_r8 / tp(:)**.41_r8
	   end where
	   tp(:ncol)          = t0 / tempe(:ncol,k)
	   rxt(:,k,elec1_ndx) = 4.e-7_r8 * tp(:)**.85_r8
	   rxt(:,k,elec3_ndx) = 1.8e-7_r8 * tp(:)**.39_r8
	   where( tp(:ncol) < 4._r8 )
	      rxt(:,k,elec2_ndx) = 2.7e-7_r8 * tp(:)**.7_r8
	   elsewhere
	      rxt(:,k,elec2_ndx) = 1.6e-7_r8 * tp(:)**.55_r8
	   end where
	end do level_loop2
     endif

     ! quenching of O+(2P) and O+(2D) by e to produce O+
     ! See TABLE 1 of Roble (1995)
     ! drm 2015-07-27
     if (elec4_ndx > 0 .and. elec5_ndx > 0 .and. elec6_ndx > 0) then
         do k=1,pver
            tp(:ncol)          = sqrt(300._r8 / tempe(:ncol,k))
            rxt(:,k,elec4_ndx) = 1.5e-7_r8 * tp(:)
            rxt(:,k,elec5_ndx) = 4.0e-8_r8 * tp(:)
            rxt(:,k,elec6_ndx) = 6.6e-8_r8 * tp(:)
         end do
     endif

!-----------------------------------------------------------------
!	... tropospheric "aerosol" rate constants
!-----------------------------------------------------------------
     if ( het1_ndx > 0 .AND. (.NOT. usr_N2O5_aer_ndx > 0) ) then
         amas = 4._r8*pi*rm1**3*den/3._r8            ! each mean particle(r=0.1u) mass (g)
         do k = 1,pver
!-------------------------------------------------------------------------
! 	... estimate humidity effect on aerosols (from Shettle and Fenn, 1979)
!           xr is a factor of the increase aerosol radii with hum (hum=0., factor=1)
!-------------------------------------------------------------------------
            xr(:)     = .999151_r8 + relhum(:ncol,k)*(1.90445_r8 + relhum(:ncol,k)*(-6.35204_r8 + relhum(:ncol,k)*5.32061_r8))
!-------------------------------------------------------------------------
! 	... estimate sulfate particles surface area (cm2/cm3) in each grid
!-------------------------------------------------------------------------
            if ( carma_hetchem_feedback ) then
               sur(:ncol) = strato_sad(:ncol,k)
            else
               sur(:) = sulfate(:,k)*m(:,k)/avo*wso4 &              ! xform mixing ratio to g/cm3
                        / amas &                                    ! xform g/cm3 to num particels/cm3
                        * fare &                                    ! xform num particels/cm3 to cm2/cm3
                        * xr(:)*xr(:)                               ! humidity factor
            endif
!-----------------------------------------------------------------
!	... compute the "aerosol" reaction rates
!-----------------------------------------------------------------
!             k = gam * A * velo/4
!
!       where velo = sqrt[ 8*bk*T/pi/(w/av) ]
!             bk = 1.381e-16
!             av = 6.02e23
!             w  = 108 (n2o5)  HO2(33)  CH2O (30)  NH3(15)
!
!       so that velo = 1.40e3*sqrt(T)  (n2o5)   gama=0.1
!       so that velo = 2.53e3*sqrt(T)  (HO2)    gama>0.2
!       so that velo = 2.65e3*sqrt(T)  (CH2O)   gama>0.022
!       so that velo = 3.75e3*sqrt(T)  (NH3)    gama=0.4
!--------------------------------------------------------
!-----------------------------------------------------------------
!	... use this n2o5 -> 2*hno3 only in tropopause
!-----------------------------------------------------------------
	    rxt(:,k,het1_ndx) = rxt(:,k,het1_ndx) &
                                +.25_r8 * gam1 * sur(:) * 1.40e3_r8 * sqrt( temp(:,k) )
         end do
      end if

!lke++
!-----------------------------------------------------------------
!      ... CO tags
!-----------------------------------------------------------------
      if( usr_CO_OH_b_ndx > 0 ) then
         if( usr_COhc_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_COhc_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_COme_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_COme_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO01_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO01_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO02_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO02_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO03_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO03_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO04_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO04_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO05_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO05_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO06_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO06_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO07_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO07_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO08_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO08_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO09_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO09_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO10_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO10_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO11_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO11_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO12_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO12_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO13_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO13_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO14_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO14_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO15_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO15_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO16_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO16_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO17_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO17_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO18_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO18_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO19_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO19_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO20_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO20_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO21_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO21_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO22_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO22_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO23_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO23_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO24_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO24_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO25_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO25_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO26_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO26_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO27_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO27_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO28_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO28_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO29_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO29_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO30_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO30_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO31_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO31_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO32_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO32_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO33_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO33_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO34_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO34_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO35_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO35_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO36_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO36_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO37_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO37_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO38_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO38_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO39_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO39_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO40_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO40_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO41_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO41_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
         if( usr_CO42_OH_ndx > 0 ) then
            rxt(:ncol,:,usr_CO42_OH_ndx) = rxt(:ncol,:,usr_CO_OH_b_ndx)
         end if
      end if
!lke--
!
! jfl : additional BAM removal reactions.  Zero out below the tropopause
!
      do l=1,num_strat_tau
!
         if ( usr_strat_tau_ndx(l) > 0 ) then
            do i=1,ncol
               rxt(i,tropchemlev(i)+1:pver,usr_strat_tau_ndx(l)) = 0._r8
            end do
         end if
!
      end do
!

      deallocate( sfc_array, dm_array )

  end subroutine usrrxt

      subroutine usrrxt_hrates( rxt, tempn, tempi, tempe, &
				h2ovmr, m, ncol, kbot )
!-----------------------------------------------------------------
!        ... set the user specified reaction rates for heating
!-----------------------------------------------------------------

      use shr_kind_mod,  only : r8 => shr_kind_r8
      use chem_mods,     only : rxntot
      use ppgrid,        only : pver, pcols

      implicit none

!-----------------------------------------------------------------
!        ... dummy arguments
!-----------------------------------------------------------------
      integer, intent(in)     :: ncol                         ! number columns in chunk
      integer, intent(in)     :: kbot                         ! heating levels
      real(r8), intent(in)    :: tempn(pcols,pver)            ! neutral temperature (K)
      real(r8), intent(in)    :: tempi(pcols,pver)            ! ion temperature (K)
      real(r8), intent(in)    :: tempe(pcols,pver)            ! electron temperature (K)
      real(r8), intent(in)    :: m(ncol,pver)                 ! total atm density (1/cm^3)
      real(r8), intent(in)    :: h2ovmr(ncol,pver)            ! water vapor (vmr)
      real(r8), intent(inout) :: rxt(ncol,pver,rxntot)        ! gas phase rates

!-----------------------------------------------------------------
!        ... local variables
!-----------------------------------------------------------------

      integer  ::  k
      real(r8), dimension(ncol) :: &
                   tp, &
                   tinv, &
                   ko, &
                   kinf, &
                   fc

!-----------------------------------------------------------------
!	... o + o2 + m --> o3 + m
!-----------------------------------------------------------------
      do k = 1,kbot
         tinv(:ncol)       = 1._r8 / tempn(:ncol,k)
         tp(:)             = 300._r8 * tinv(:)
         rxt(:,k,usr_O_O2_ndx) = 6.e-34_r8 * tp(:)**2.4_r8

!-----------------------------------------------------------------
!	... o + o + m -> o2 + m
!-----------------------------------------------------------------
         rxt(:,k,usr_O_O_ndx) = 2.76e-34_r8 * exp( 720.0_r8*tinv(:) )

!-----------------------------------------------------------------
!	... ho2 + ho2 --> h2o2
!	Note: this rate involves the water vapor number density
!-----------------------------------------------------------------
         ko(:)   = 3.0e-13_r8  * exp( 460._r8*tinv(:) )
         kinf(:) = 2.1e-33_r8 * m(:,k) * exp( 920._r8*tinv(:) )
         fc(:)   = 1._r8 + 1.4e-21_r8 * m(:,k) * h2ovmr(:,k) * exp( 2200._r8*tinv(:) )
         rxt(:,k,usr_HO2_HO2_ndx) = (ko(:) + kinf(:)) * fc(:)

      end do

!-----------------------------------------------------------------
! 	... the ionic rates
!-----------------------------------------------------------------
      if ( has_ion_rxts ) then
         level_loop2 :  do k = 1,kbot
            tp(:ncol)         = (2._r8*tempi(:ncol,k) + tempn(:ncol,k)) / ( 3._r8 * t0 )
            tp(:)             = max( min( tp(:),20._r8 ),1._r8 )
            rxt(:,k,ion1_ndx) = 2.82e-11_r8 + tp(:)*(-7.74e-12_r8 + tp(:)*(1.073e-12_r8  &
                 + tp(:)*(-5.17e-14_r8 + 9.65e-16_r8*tp(:))))
            tp(:ncol)         = (.6363_r8*tempi(:ncol,k) + .3637_r8*tempn(:ncol,k)) / t0
            tp(:)             = max( min( tp(:),trlim2 ),1._r8 )
            rxt(:,k,ion2_ndx) = 1.533e-12_r8 + tp(:)*(-5.92e-13_r8 + tp(:)*8.6e-14_r8)
            tp(:ncol)         = 2._r8 * t0 /(tempi(:ncol,k) + tempn(:ncol,k))
            where( tp(:ncol) < trlim3 )
               rxt(:,k,ion3_ndx)  = 1.4e-10_r8 * tp(:)**.44_r8
            elsewhere
               rxt(:,k,ion3_ndx)  = 5.2e-11_r8 / tp(:)**.2_r8
            endwhere
            tp(:ncol)          = t0 / tempe(:ncol,k)
            rxt(:,k,elec1_ndx) = 4.e-7_r8 * tp(:)**.85_r8
            rxt(:,k,elec3_ndx) = 1.8e-7_r8 * tp(:)**.39_r8
            where( tp(:ncol) < 4._r8 )
               rxt(:,k,elec2_ndx) = 2.7e-7_r8 * tp(:)**.7_r8
            elsewhere
               rxt(:,k,elec2_ndx) = 1.6e-7_r8 * tp(:)**.55_r8
            endwhere
         end do level_loop2
      endif
      end subroutine usrrxt_hrates

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
  subroutine comp_exp( x, y, n )

    implicit none

    real(r8), intent(out) :: x(:)
    real(r8), intent(in)  :: y(:)
    integer,  intent(in)  :: n

#ifdef IBM
    call vexp( x, y, n )
#else
    x(:n) = exp( y(:n) )
#endif

  end subroutine comp_exp

  !-------------------------------------------------------------------------
  !  Heterogeneous reaction rates for uptake of a gas on an aerosol:
  !-------------------------------------------------------------------------
  function hetrxtrate( sfc, dm_aer, dg_gas, c_gas, gamma_gas ) result(rate)

    real(r8), intent(in) :: sfc(:)
    real(r8), intent(in) :: dm_aer(:)
    real(r8), intent(in) :: dg_gas
    real(r8), intent(in) :: c_gas
    real(r8), intent(in) :: gamma_gas
    real(r8) :: rate

    real(r8),allocatable :: rxt(:)
    integer :: n, i

    n = size(sfc)

    allocate(rxt(n))
    do i=1,n
       rxt(i) = sfc(i) / (0.5_r8*dm_aer(i)/dg_gas + (4._r8/(c_gas*gamma_gas)))
    enddo

    rate = sum(rxt)

    deallocate(rxt)

  endfunction hetrxtrate

  !-------------------------------------------------------------------------
  !  Heterogeneous reaction rates for uptake of a glyoxal gas on an aerosol:
  !-------------------------------------------------------------------------
  function hetrxtrate_gly( sfc, c_gas, gamma_gas ) result(rate)

    real(r8), intent(in) :: sfc(:)
    real(r8), intent(in) :: c_gas
    real(r8), intent(in) :: gamma_gas
    real(r8) :: rate

    real(r8),allocatable :: rxt(:)
    integer :: n, i

    n = size(sfc)

    allocate(rxt(n))
    do i=1,n
       rxt(i) =  0.25_r8 * c_gas * sfc(i) * gamma_gas
    enddo

    rate = sum(rxt)

    deallocate(rxt)

  endfunction hetrxtrate_gly
  
  ! ------------------------------------------------
  ! WSY: Phase-transfer coefficient, on wet aerosols
  ! Siyuan Wang (siyuan@ucar.edu)
  ! ------------------------------------------------
  function kt_s_wetaerosols(ncol, radius_cm, dg_cm2_s, c_cm_s, alpha)
    
    real(r8), intent(in) :: radius_cm(ncol)        ! particle rdius (cm)
    real(r8), intent(in) :: dg_cm2_s               ! gas-phase diffusion coefficient (cm2/s)
    real(r8), intent(in) :: c_cm_s(ncol)           ! thermal speed (cm/s)
    real(r8), intent(in) :: alpha                  ! mass accommodation coefficient (dimension-less)
    
    real(r8)             :: kt_s_wetaerosols(ncol) ! phase-transfer coefficient (s^-1)
    
    integer              :: ncol                   ! col index
    
    kt_s_wetaerosols = 1._r8/(radius_cm*radius_cm/3._r8/dg_cm2_s + (4._r8/3._r8)*radius_cm/c_cm_s/alpha)
    
  endfunction kt_s_wetaerosols
  
  ! ---------------------------------------------------------------------  
  ! WSY: this calculates the effective Henry's law constant for a acidity
  !      using the Henry's law constant look-up table in seq_drydep_mod
  !      for bases, formulation is slightly different, see:
  !      https://wiki.ucar.edu/display/camchem/Updating+Gas-Phase+Chemistry
  ! Siyuan Wang (siyuan@ucar.edu)
  ! ---------------------------------------------------------------------
  function heff_acid_lookup(SpeciesName, temp_k, pH, do_i_look_like_an_acid)
    
    character(Len=16), intent(in)   :: SpeciesName
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH
    logical,           intent(in)   :: do_i_look_like_an_acid
    
    real(r8)                        :: heff_acid_lookup
    real(r8)                        :: kh, kh298, dhr, k1, k1_298,dh1r, k2, k2_298, dh2r, aH
    
    integer                         :: i
    
    do i=1,n_species_table
      if (trim(SpeciesName)==trim(species_name_table(i))) then
        ! col 1-6 of dheff array...
        kh298  = dheff((i-1)*6+1)
        dhr    = dheff((i-1)*6+2)
        k1_298 = dheff((i-1)*6+3)
        dh1r   = dheff((i-1)*6+4)
        k2_298 = dheff((i-1)*6+5)
        dh2r   = dheff((i-1)*6+6)
        ! formulation: https://wiki.ucar.edu/display/camchem/Updating+Gas-Phase+Chemistry
        kh = kh298*exp(dhr*(1.0_r8/temp_k-1.0_r8/298.0_r8))
        aH = 10.0_r8**(-1.0_r8*pH)
        k1 = k1_298 * exp(dh1r*(1.0_r8/temp_k-1.0_r8/298.0_r8))
        k2 = k2_298 * exp(dh2r*(1.0_r8/temp_k-1.0_r8/298.0_r8))
        if (do_i_look_like_an_acid) then
          heff_acid_lookup = kh * (1.0_r8 + k1/aH * (1.0_r8 + k2/aH))  ! acid formulation
        else
          heff_acid_lookup = kh * (1.0_r8 + k1*aH/k2)                  ! base formulation
        end if
        ! write(*,*) "--- species: ", trim(SpeciesName), trim(species_name_table(i))
        ! write(*,*) "--- h298:    ", dheff((i-1)*6+1)
        ! write(*,*) "--- dhr:     ", dheff((i-1)*6+2)
        exit
      end if
    end do
    
  endfunction heff_acid_lookup
  
  ! ---------------------------------------------
  ! WSY: aqueous-phase reaction: SO2 + H2O2 = SO4
  ! Siyuan Wang (siyuan@ucar.edu)
  ! ---------------------------------------------
  function k_H2O2_SO2_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_H2O2_SO2_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: K1_SO2, K2_SO2, f_HSO3m, aH
    
    aH = 10.0_r8**(-1.0_r8*pH)
    K1_SO2 = 1.3E-2_r8 * exp(1960.0_r8*(1.0_r8/temp_k - 1.0_r8/298.0_r8))
    K2_SO2 = 6.6E-8_r8 * exp(1500.0_r8*(1.0_r8/temp_k - 1.0_r8/298.0_r8))
    f_HSO3m = K1_SO2*aH / (aH*aH + K1_SO2*aH + K1_SO2*K2_SO2)
    k_H2O2_SO2_M_s = 7.45E+7_r8 * aH / (1.0_r8 + 13.0_r8*aH) * f_HSO3m 
    
  endfunction k_H2O2_SO2_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: DMS + O3 = DMSO
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_DMS_O3_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_DMS_O3_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    
    k298 = 8.61e8_r8
    EaR  = -2600._r8
    k_DMS_O3_M_s = k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) )
    
  endfunction k_DMS_O3_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: DMSO + OH -> MSIA
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_DMSO_OH_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_DMSO_OH_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    
    k298 = 6.63e9_r8
    EaR  = -1270._r8
    k_DMSO_OH_M_s = k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) )
    
  endfunction k_DMSO_OH_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: MSIA + OH -> MSA
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_MSIA_OH_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_MSIA_OH_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    real(r8)                        :: k1, EaR1    ! reaction rates and temperature dependent of MSI- + OH
    real(r8)                        :: Ka    ! acidic assocation of MSIA
    real(r8)                        :: H  ! [H+]
    
    k298 = 6.e9_r8
    EaR  = 0._r8
    k1   = 1.2e10_r8
    EaR1 = 0._r8
    Ka   = 10._r8**(-2.28_r8)
    H    = 10.0_r8**(-1.0_r8*pH)
    k_MSIA_OH_M_s = k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) ) + &
                         k1 * EXP( -EaR1 * (1._r8 / temp_k - 1._r8 / 298._r8) ) * Ka / (Ka + H)
    
  endfunction k_MSIA_OH_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: MSIA + O3 -> MSA
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_MSIA_O3_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_MSIA_O3_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    real(r8)                        :: k1, EaR1    ! reaction rates and temperature dependent of MSI- + O3
    real(r8)                        :: Ka    ! acidic assocation of MSIA
    real(r8)                        :: H  ! [H+]
    
    k298 = 3.5e7_r8
    EaR  = 0._r8
    k1   = 2.e6_r8
    EaR1 = 0._r8
    Ka   = 10._r8**(-2.28_r8)
    H    = 10.0_r8**(-1.0_r8*pH)
    k_MSIA_O3_M_s = k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) ) + &
                         k1 * EXP( -EaR1 * (1._r8 / temp_k - 1._r8 / 298._r8) ) * Ka / (Ka + H)
    
  endfunction k_MSIA_O3_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: MSA + OH -> so4
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_MSA_OH_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_MSA_OH_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    real(r8)                        :: k1, EaR1    ! reaction rates and temperature dependent of MS- + OH
    real(r8)                        :: Ka    ! acidic assocation of MSA
    real(r8)                        :: H  ! [H+]
    
    k298 = 1.5e7_r8
    EaR  = 0._r8
    k1   = 1.29e7_r8
    EaR1 = -2630._r8
    Ka   = 10._r8**(1.86_r8)
    H    = 10.0_r8**(-1.0_r8*pH)
    k_MSA_OH_M_s = k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) ) + &
                         k1 * EXP( -EaR1 * (1._r8 / temp_k - 1._r8 / 298._r8) ) * Ka / (Ka + H)
    
  endfunction k_MSA_OH_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: HSO3- + HOBR -> so4 + 2H + Br & SO3-- + HOBR -> so4 + 2H + Br
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_SIV_HOBR_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_SIV_HOBR_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    real(r8)                        :: k1, EaR1    ! reaction rates and temperature dependent of MS- + OH
    real(r8)                        :: Ka1, Ka2    ! acidic assocation of S(IV)
    real(r8)                        :: H  ! [H+]
    
    k298 = 3.2e9_r8
    EaR  = 0._r8
    k1   = 5.0e9_r8
    EaR1 = 0._r8
    Ka1  = 1.30e-02_r8 * EXP( -1960._r8 * (1._r8 / temp_k - 1._r8 / 298._r8) )
    Ka2  = 6.6e-08_r8  * EXP( -1500._r8 * (1._r8 / temp_k - 1._r8 / 298._r8) )
    H    = 10.0_r8**(-1.0_r8*pH)
    k_SIV_HOBR_M_s = ( ( k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) ) * Ka1 / H )    + &
                      ( k1 * EXP( -EaR1 * (1._r8 / temp_k - 1._r8 / 298._r8) ) * (Ka1 * Ka2 / H**2) )  & 
                      ) / (1 + Ka1 / H + Ka1 * Ka2 / H**2)
    
  endfunction k_SIV_HOBR_M_s
  
  ! ---------------------------------------------
  ! fkm: aqueous-phase reaction: HSO3- + O3 -> so4 + 2H + O2 & SO3-- + O3 -> so4 + O2
  ! Ka Ming Fung (fkm@mit.edu)
  ! Ref: Chen (2018) ACP
  ! ---------------------------------------------
  function k_SIV_O3_M_s(pH, temp_k)
    
    real(r8),          intent(in)   :: temp_k
    real(r8),          intent(in)   :: pH  
    
    real(r8)                        :: k_SIV_O3_M_s  ! unit: M^-1 s^-1
    real(r8)                        :: k298  ! base reaction rate at T = 298 K [M s-1]
    real(r8)                        :: EaR   ! temperature dependent coefficient for reaction rates [K]
    real(r8)                        :: k1, EaR1    ! reaction rates and temperature dependent of SO3 2- + O3 -> SO4 2- + O2
    real(r8)                        :: Ka1, Ka2    ! acidic assocation of S(IV)
    real(r8)                        :: H  ! [H+]
    
    k298 = 3.2e5_r8
    EaR  = 4830._r8
    k1   = 1.0e9_r8
    EaR1 = 4030._r8
    Ka1  = 1.30e-02_r8 * EXP( -1960._r8 * (1._r8 / temp_k - 1._r8 / 298._r8) )
    Ka2  = 6.6e-08_r8  * EXP( -1500._r8 * (1._r8 / temp_k - 1._r8 / 298._r8) )
    H    = 10.0_r8**(-1.0_r8*pH)
    k_SIV_O3_M_s = ( ( k298 * EXP( -EaR * (1._r8 / temp_k - 1._r8 / 298._r8) ) * Ka1 / H )    + &
                      ( k1 * EXP( -EaR1 * (1._r8 / temp_k - 1._r8 / 298._r8) ) * (Ka1 * Ka2 / H**2) )  & 
                      ) / (1 + Ka1 / H + Ka1 * Ka2 / H**2)
    
  endfunction k_SIV_O3_M_s
  
end module mo_usrrxt
