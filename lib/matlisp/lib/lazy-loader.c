/*
 * Foreign functions needed by Matlisp.  The purpose of this file is
 * to declare all of the foreign functions used by Matlisp.  We don't
 * need to get the return types or even the arguments right.  We just
 * need the names.  The return types and argument types are derived
 * from Lisp code.
 */

/*
 * Special functions from SPECFUN (TOMS 748)
 */
extern double anorm_();
extern double besei0_();
extern double besei1_();
extern double besek0_();
extern double besek1_();
extern double besi0_();
extern double besi1_();
extern double besj0_();
extern double besj1_();
extern double besk0_();
extern double besk1_();
extern double besy0_();
extern double besy1_();
extern double daw_();
extern double derf_();
extern double derfc_();
extern double derfcx_();
extern double dgamma_();
extern double dlgama_();
extern double ei_();
extern double eone_();
extern double expei_();
extern double psi_();
extern double ribesl_();
extern double rjbesl_();
extern double rkbesl_();
extern double rybesl_();

/*
 * Main routines from FFTPACK
 */
extern int zffti_();
extern int zfftf_();
extern int zfftb_();

/*
 * BLAS and LAPACK routines (only the ones we have foreign function
 * interfaces).
 */

extern int dgesv_(); extern int dgeev_(); extern int dgetrf_(); extern int dgesvd_();
extern int zgesv_(); extern int zgeev_(); extern int zgetrf_(); extern int zgesvd_();
extern int idamax_(); extern int dasum_(); extern int ddot_(); extern int dnrm2_();
extern int dcabs1_(); extern int dzasum_(); extern int dznrm2_(); extern int izamax_();
extern int drot_(); extern int dscal_(); extern int dswap_(); extern int dcopy_(); extern int daxpy_();
extern int zdscal_(); extern int zscal_(); extern int zswap_(); extern int zcopy_(); extern int zaxpy_(); extern int zdotc_(); extern int zdotu_();
extern int dgemv_(); extern int dsymv_(); extern int dtrmv_(); extern int dtrsv_(); extern int dger_(); extern int dsyr_(); extern int dsyr2_();
extern int zgemv_(); extern int zhemv_(); extern int ztrmv_(); extern int ztrsv_(); extern int zgerc_(); extern int zgeru_(); extern int zher2_();
extern int dgemm_(); extern int dsyrk_(); extern int dsyr2k_(); extern int dtrmm_(); extern int dtrsm_();
extern int dsymm_();
extern int zgemm_(); extern int ztrmm_(); extern int ztrsm_(); extern int zherk_(); extern int zher2k_();

extern int dgetrs_();
extern int zgetrs_();
/*
 * Just make sure all functions are used
 */
static void
__lazy_loader__(void)
{
    zffti_();
    zfftf_();
    zfftb_();

    dgesv_(); dgeev_(); dgetrf_(); dgesvd_();
    zgesv_(); zgeev_(); zgetrf_(); zgesvd_();
    idamax_(); dasum_(); ddot_(); dnrm2_();
    dcabs1_(); dzasum_(); dznrm2_(); izamax_();
    drot_(); dscal_(); dswap_(); dcopy_(); daxpy_();
    zdscal_(); zscal_(); zswap_(); zcopy_(); zaxpy_(); zdotc_(); zdotu_();
    dgemv_(); dsymv_(); dtrmv_(); dtrsv_(); dger_(); dsyr_(); dsyr2_();
    zgemv_(); zhemv_(); ztrmv_(); ztrsv_(); zgerc_(); zgeru_(); zher2_();
    dgemm_(); dsyrk_(); dsyr2k_(); dtrmm_(); dtrsm_();
    dsymm_();
    zgemm_(); ztrmm_(); ztrsm_(); zherk_(); zher2k_();

    dgetrs_(); zgetrs_();
    
    anorm_();
    besei0_();
    besei1_();
    besek0_();
    besek1_();
    besi0_();
    besi1_();
    besj0_();
    besj1_();
    besk0_();
    besk1_();
    besy0_();
    besy1_();
    daw_();
    derf_();
    derfc_();
    derfcx_();
    dgamma_();
    dlgama_();
    ei_();
    eone_();
    expei_();
    psi_();
    ribesl_();
    rjbesl_();
    rkbesl_();
    rybesl_();
}
