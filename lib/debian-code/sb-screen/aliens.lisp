(in-package :sb-screen)

(load-shared-object "libslang.so.2")

;;; SLtt functions

(define-alien-routine "SLtt_get_terminfo" void)
(define-alien-variable "SLtt_Use_Ansi_Colors" long)
(define-alien-variable "SLtt_Screen_Rows" long)
(define-alien-variable "SLtt_Screen_Cols" long)
(define-alien-routine "SLtt_set_color" void (obj long) (name c-string) (fg c-string) (bg c-string))
(define-alien-routine "SLtt_get_screen_size" void)
(define-alien-routine "SLtt_tgetstr" c-string (char c-string))

;;; SLkp functions

(define-alien-routine "SLkp_init" int)
(define-alien-routine "SLkp_define_keysym" long (key c-string) (sym unsigned-long))
(define-alien-routine "SLkp_getkey" long)

;;; SLang functions

(define-alien-routine "SLang_init_tty" void (abort-char-code long) (flow-control long) (output-processing long))
(define-alien-routine "SLang_input_pending" long (time long))
(define-alien-routine "SLang_reset_tty" void)
(define-alien-routine "SLang_getkey" long)
(define-alien-routine "SLang_process_keystring" c-string (keystring c-string))

;;; SLsmg functions

(define-alien-routine "SLsmg_init_smg" long)
(define-alien-routine "SLsmg_cls" void)
(define-alien-routine "SLsmg_gotorc" void (row long) (col long))
(define-alien-routine "SLsmg_write_string" void (string c-string))
(define-alien-routine "SLsmg_refresh" void)
(define-alien-routine "SLsmg_reset_smg" void)
(define-alien-routine "SLsmg_draw_hline" void (len unsigned-long))
(define-alien-routine "SLsmg_draw_vline" void (len long))
(define-alien-routine "SLsmg_set_char_set" void (set long))
(define-alien-routine "SLsmg_erase_eol" void)
(define-alien-routine "SLsmg_erase_eos" void)
(define-alien-routine "SLsmg_set_color" void (obj int))
(define-alien-routine "SLsmg_reinit_smg" void)
(define-alien-routine "SLsmg_get_column" long)
(define-alien-routine "SLsmg_get_row" long)
