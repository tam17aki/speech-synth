;;; speech-synth.el --- A wrapper for Flite+hts_engine and Open JTalk.

;; Copyright (C) 2013 by Akira Tamamori

;; Author: Akira TAMAMORI
;; Version: 1.2.1
;; Package-Requires: ((deferred "0.3.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `speech-synth.el' is a wrapper for two HMM-based speech synthesis
;; engines, Flite+hts_engine (English TTS system) and Open JTalk
;; (Japanese TTS system). This package provides a major mode and
;; several commands to perform speech synthesis on Emacs.

;;; Installation:
;;
;; In advance, please install hts_engine-API, Flite+hts_engine and
;; Open JTalk:
;;
;; - hts_engine-API and Flite+hts_engine
;;   [http://hts-engine.sourceforge.net]
;;
;; - Open JTalk
;;   [http://open-jtalk.sourceforge.net]
;;
;; Please confirm the version numbers of the above softwares:
;; hts_engine-API >= 1.07, Flite+hts_engine >= 1.04 and Open JTalk >= 1.06.
;; From the above sites, you should also download HTS voice files.
;; Please be sure to install the HTS voice files in the same directory.
;; (ex. "/usr/local/share/hts_engine")
;;
;; deferred.el is required for asynchronous process.
;; [http://github.com/kiwanami/emacs-deferred/blob/master/deferred.el].
;;
;; sox is also required to play synthesized waveform.
;; [http://sox.sourceforge.net].
;;
;; Next, put speech-synth.el in your load-path and add followings
;; to your `user-init-file':
;;
;; ----------------------------------------------------------------------------
;; (require 'speech-synth)
;; ----------------------------------------------------------------------------
;;
;; You must also specify correct path names in accordance with your
;; environment:
;;
;; ----------------------------------------------------------------------------
;; (setq speech-synth-dictionary-directory "/usr/local/share/dic")
;; (setq speech-synth-voice-directory "/usr/local/share/hts_voice/")
;; (setq speech-synth-Flite-voice-file
;;       (concat speech-synth-voice-directory "cmu_us_arctic_slt.htsvoice"))
;; (setq speech-synth-OpenJTalk-voice-file
;;       (concat speech-synth-voice-directory "nitech_jp_atr503_m001.htsvoice"))
;; ----------------------------------------------------------------------------
;;
;; If you can prepare and install HTS voice files for emotional
;; speech synthesis, you should add following:
;;
;; ----------------------------------------------------------------------------
;; (setq speech-synth-emotional-speech t)
;; ----------------------------------------------------------------------------

;;; Commands:
;;
;; `speech-synth'
;;     Generate *Speech Synth* buffer to input text for speech synthesis.
;;
;; `speech-synth-from-region'
;;     Synthesize speech from strings in region.
;;     When `speech-synth-auto-select-language' is t,
;;     language (English or Japanese) in speech synthesis is automatically
;;     selected.
;;
;; `speech-synth-from-buffer'
;;     Synthesize speech from strings in buffer.
;;     When `speech-synth-auto-select-language' is t,
;;     language (English or Japanese) in speech synthesis is automatically
;;     selected.
;;
;; `speech-synth-set-emotion'
;;     Specify emotion in speech synthesis (Japanese only).
;;     When invoking this command with C-u, emotion is reset to default.
;;     Of cource you must prepare HTS voice files for each emotion.
;;
;; `speech-synth-set-parameter'
;;     Set parameters in speech synthesis.
;;     When invoking this command with C-u, all parameters are reset to their
;;     default values.

;;; ChangeLog:
;;
;; 1.2.1
;;   * fix the maximum and minimum values of speech synthesis parameters.
;;
;; 1.2.0
;;   * update document for installation.
;;   * modify function `speech-synth' to possess multiple *Speech Synth* buffers.
;;   * change implimentations of `speech-synth-from-buffer',
;;     `speech-synth-from-region', `speech-synth-execute-synthesis' and
;;     `speech-synth-get-string'.
;;   * remove functions of `speech-synth-japanese-from-buffer',
;;     `speech-synth-japanese-from-region', `speech-synth-english-from-buffer',
;;     and `speech-synth-english-from-region', `speech-synth-popup'.
;;   * change default values of `speech-synth-intonation-default' and
;;     `speech-synth-postfilter-default'.
;;   * add new variables of `speech-synth-vu-threshold-default' and
;;     `speech-synth-vu-threshold' to specify voiced/unvoiced threshold.
;;   * change variable name of `speech-synth-buffer' to `speech-synth-buffer-name'.
;;   * normalize sound volume when playing wav file of synthesized speech.
;;
;; 1.1.0
;;   * remove redundant arguments from several functions.
;;   * add functions to synthesise emotional speech (Japanese only).
;;   * add variables to specify voice volume, intonation, and post-filtering.
;;
;; 1.0.0
;;   * Initial release.

;;; Code:

(eval-when-compile (require 'cl))
(require 'view)
(require 'deferred)

;;;; Customization

(defgroup speech-synth nil
  "speech synthesis."
  :group 'application
  :prefix "speech-synth-")

(defcustom speech-synth-dictionary-directory "/usr/local/share/dic/"
  "Dictionary directory for Open JTalk."
  :type 'directory
  :group 'speech-synth)

(defcustom speech-synth-voice-directory "/usr/local/share/hts_voice/"
  "HTS voice directory."
  :type 'directory
  :group 'speech-synth)

(defcustom speech-synth-Flite-voice-file
  (concat speech-synth-voice-directory
          "cmu_us_arctic_slt.htsvoice")
  "HTS voice file for Flite+hts_engine."
  :type '(file :must-match t)
  :group 'speech-synth)

(defcustom speech-synth-OpenJTalk-voice-file
  (concat speech-synth-voice-directory
          "nitech_jp_atr503_m001.htsvoice")
  "HTS voice file for Open JTalk."
  :type '(file :must-match t)
  :group 'speech-synth)

(defcustom speech-synth-Flite-command (executable-find "flite_hts_engine")
  "Flite+hts_engine command."
  :type 'file
  :group 'speech-synth)

(defcustom speech-synth-OpenJTalk-command (executable-find "open_jtalk")
  "Open JTalk command."
  :type 'file
  :group 'speech-synth)

(defcustom speech-synth-play-wav t
  "Non-nil means synthesized waveform is played."
  :type 'boolean
  :group 'speech-synth)

(defcustom speech-synth-lang-select-percent 40.0
  "Percentage for language auto-selection."
  :type 'float
  :group 'speech-synth)

(defcustom speech-synth-auto-select-language t
  "If non-nil, language in speech synthesis is selected automatically."
  :type 'boolean
  :group 'speech-synth)

(defcustom speech-synth-emotional-speech nil
  "If non-nil, emotional speech synthesis can be used (Japanese only).
Of cource, you must prepare HTS voice files for each emotion."
  :type 'boolean
  :group 'speech-synth)

(defcustom speech-synth-maximum-character-number-English 256
  "Maximum number of character that can be synthesized in English."
  :type 'integer
  :group 'speech-synth)

(defcustom speech-synth-maximum-character-number-Japanese 128
  "Maximum number of character that can be synthesized in Japanese."
  :type 'integer
  :group 'speech-synth)

;;;; Internal variables

(defvar speech-synth-mode-name "Synth"
  "Major mode name displayed in mode line.")

(defvar speech-synth-buffer-name "*Speech Synth*"
  "Buffer name for speech-synth-mode.")

(defvar speech-synth-emotion-prefix "mei_"
  "Prefix of voice file for emotinal speech synthesis.")

(defvar speech-synth-language "English"
  "Default language in speech speech.")

(defvar speech-synth-spectral-warping 0.55)
(make-variable-buffer-local 'speech-synth-spectral-warping)

(defvar speech-synth-speaking-rate 1.0)
(make-variable-buffer-local 'speech-synth-speaking-rate)

(defvar speech-synth-pitch-shift 0.0)
(make-variable-buffer-local 'speech-synth-pitch-shift)

(defvar speech-synth-voice-volume 1.0)
(make-variable-buffer-local 'speech-synth-voice-volume)

(defvar speech-synth-intonation 0.66)
(make-variable-buffer-local 'speech-synth-intonation)

(defvar speech-synth-postfilter 0.1)
(make-variable-buffer-local 'speech-synth-postfilter)

(defvar speech-synth-vu-threshold 0.23)
(make-variable-buffer-local 'speech-synth-vu-threshold)

(defvar speech-synth-emotion "normal"
  "Default emotion in emotional speech speech.")
(make-variable-buffer-local 'speech-synth-emotion)

(defvar speech-synth-mode-key-table
  '(("C-c C-r"   . speech-synth-from-region)
    ("C-c C-b"   . speech-synth-from-buffer)
    ("C-c C-q"   . speech-synth-quit))
  "Default key table for speech-synth-mode.")

(defvar speech-synth-language-list '("English" "Japanese"))

(defvar speech-synth-parameter-list
  '("Spectral warping" "Speech rate" "Pitch-shift" "Voice volume"
    "Intonation" "Postfilter" "Voiced/Unvoiced"))

(defvar speech-synth-emotion-list '("normal" "happy" "sad" "angry" "bashful"))

;;;; Major mode

(define-derived-mode speech-synth-mode fundamental-mode speech-synth-mode-name
  "Major mode for speech synthesis."
  :group 'speech-synth
  (loop for (key . cmd) in speech-synth-mode-key-table
        do (define-key speech-synth-mode-map (read-kbd-macro key) cmd)))

;;;; Internal functions

(defun speech-synth-quit ()
  (interactive)
  (view-mode 1) (View-quit))

(defun speech-synth-get-language (text)
  (cond
   ((> (/ (* (length (replace-regexp-in-string "[^A-Za-z 0-9]+" "" text))
             100)
          (length text))
       speech-synth-lang-select-percent)
    "English")
   (t
    "Japanese")))

(defun speech-synth-get-string (start end)
  (let* ((text (buffer-substring-no-properties start end))
         (lang (speech-synth-get-language text)))
    (cond ((string= lang "English")
           (if (>= (length text) speech-synth-maximum-character-number-English)
               (error "Length of input text, %s is too long to synthesize speech!"
                      (length text)))
           (shell-quote-argument
            (replace-regexp-in-string "\n" " " text)))
          ((string= lang "Japanese")
           (if (>= (length text) speech-synth-maximum-character-number-Japanese)
               (error "Length of input text, %s is too long to synthesize speech!"
                      (length text)))
           (shell-quote-argument
            (replace-regexp-in-string "\n" "" text))))))

(defun speech-synth-get-temporary-wav-file ()
  (concat (make-temp-name
           (expand-file-name temporary-file-directory)) ".wav"))

(defun speech-synth-get-command-line (lang wav)
  (cond ((string= lang "English")
         (unless speech-synth-Flite-command
           (error "You must install Flite+hts_engine!"))
         (format "%s -o %s -m %s -a %f -r %f -fm %f -jm %f -jf %f -b %f -u %f"
                 speech-synth-Flite-command
                 wav
                 speech-synth-Flite-voice-file
                 speech-synth-spectral-warping
                 speech-synth-speaking-rate
                 speech-synth-pitch-shift
                 speech-synth-voice-volume
                 speech-synth-intonation
                 speech-synth-postfilter
                 speech-synth-vu-threshold))
        ((string= lang "Japanese")
         (unless speech-synth-OpenJTalk-command
           (error "You must install Open JTalk!"))
         (format "%s -x %s -ow %s -m %s -a %f -r %f -fm %f -jm %f -jf %f -b %f -u %f"
                 speech-synth-OpenJTalk-command
                 speech-synth-dictionary-directory
                 wav
                 (if speech-synth-emotional-speech
                     (concat speech-synth-voice-directory
                             speech-synth-emotion-prefix
                             speech-synth-emotion ".htsvoice")
                   speech-synth-OpenJTalk-voice-file)
                 speech-synth-spectral-warping
                 speech-synth-speaking-rate
                 speech-synth-pitch-shift
                 speech-synth-voice-volume
                 speech-synth-intonation
                 speech-synth-postfilter
                 speech-synth-vu-threshold))
        (t
         (error "You must specify either English or Japanese!"))))

(defun speech-synth-set-parameter-reset ()
  (setq speech-synth-spectral-warping (default-value 'speech-synth-spectral-warping)
        speech-synth-speaking-rate (default-value 'speech-synth-speaking-rate)
        speech-synth-pitch-shift (default-value 'speech-synth-pitch-shift)
        speech-synth-voice-volume (default-value 'speech-synth-voice-volume)
        speech-synth-intonation (default-value 'speech-synth-intonation)
        speech-synth-postfilter (default-value 'speech-synth-postfilter)
        speech-synth-vu-threshold (default-value 'speech-synth-vu-threshold))
  (message "All speech synthesis parameters are reset."))

;;;###autoload
(defun speech-synth-set-parameter-interactive (arg)
  (interactive "P")
  (if arg
      (speech-synth-set-parameter-reset)
    (let ((param (completing-read "Parameter: " speech-synth-parameter-list nil t)))
      (cond ((string= param "Spectral warping")
             (let ((warp (read-number "Spectral warping: "
                                      (default-value 'speech-synth-spectral-warping))))
               (when (or (< warp -0.8) (> warp 0.8))
                 (error "Warping parameter must be between -0.8 and 0.8!"))
               (setq speech-synth-spectral-warping warp)))
            ((string= param "Speech rate")
             (let ((rate (read-number "Speech rate: "
                                      (default-value 'speech-synth-speaking-rate))))
               (when (or (> rate 2.0) (< rate 0.5))
                 (error "Speaking rate parameter must be between 0.5 and 2.0!"))
               (setq speech-synth-speaking-rate rate)))
            ((string= param "Pitch-shift")
             (let ((pitch (read-number "Pitch-shift: "
                                       (default-value 'speech-synth-pitch-shift))))
               (when (or (> pitch 24.0) (< pitch -24.0))
                 (error "Pitch-shift parameter must be between -24.0 and 24.0!"))
               (setq speech-synth-pitch-shift pitch)))
            ((string= param "Voice volume")
             (let ((volume (read-number "Voice volume: "
                                        (default-value 'speech-synth-voice-volume))))
               (when (< volume 0.0)
                 (error "Voice volume parameter must be greater than 0.0!"))
               (setq speech-synth-voice-volume volume)))
            ((string= param "Intonation")
             (let ((intonation (read-number "Intonation: "
                                            (default-value 'speech-synth-intonation))))
               (when (< intonation 0.0)
                 (error "Intonation parameter must be greater than 0.0!"))
               (setq speech-synth-intonation intonation)))
            ((string= param "Postfilter")
             (let ((postfilter (read-number "Postfilter: "
                                            (default-value 'speech-synth-postfilter))))
               (when (or (> postfilter 1.0) (< postfilter 0.0))
                 (error "Postfilter parameter must be between 0.0 and 1.0!"))
               (setq speech-synth-postfilter postfilter)))
            ((string= param "Voiced/Unvoiced")
             (let ((vu (read-number "Voiced/Unvoiced threshold: "
                                      (default-value 'speech-synth-vu-threshold))))
               (when (or (> vu 1.0) (< vu 0.0))
                 (error "Voiced/Unvoiced threshold parameter must be between 0.0 and 1.0!"))
               (setq speech-synth-vu-threshold vu)))))))

(defun speech-synth-set-parameter (param param_val &optional reset_all)
  (if reset_all
      (speech-synth-set-parameter-reset))
  (cond ((string= param "Spectral warping")
         (when (or (> param_val 0.8) (< param_val -0.8))
           (error "Warping parameter must be between -0.8 and 0.8!"))
         (setq speech-synth-spectral-warping param_val))
        ((string= param "Speech rate")
         (when (or (> param_val 1.0) (< param_val 0.0))
           (error "Speaking rate parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-speaking-rate param_val))
        ((string= param "Pitch-shift")
         (when (or (> param_val 24.0) (< param_val -24.0))
           (error "Pitch-shift parameter must be between -24.0 and 24.0!"))
         (setq speech-synth-pitch-shift param_val))
        ((string= param "Voice volume")
         (when (< param_val 0.0)
           (error "Voice volume parameter must be greater than 0.0!"))
         (setq speech-synth-voice-volume param_val))
        ((string= param "Intonation")
         (when (< param_val 0.0)
           (error "Intonation parameter must be greater than 0.0!"))
         (setq speech-synth-intonation param_val))
        ((string= param "Postfilter")
         (when (or (> param_val 1.0) (< param_val 0.0))
           (error "Postfilter parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-postfilter param_val))
        ((string= param "Voiced/Unvoiced")
         (when (or (> param_val 1.0) (< param_val 0.0))
           (error "Voiced/Unvoiced threshold parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-vu-threshold param_val))
        (t
         (error "Wrong type parameter is specified!"))))

;;;###autoload
(defun speech-synth-set-emotion ()
  (interactive)
  (if (and speech-synth-emotional-speech (equal speech-synth-language "Japanese"))
      (let ((emotion (completing-read "Emotion: " speech-synth-emotion-list nil t)))
        (setq speech-synth-emotion emotion)
        (message "Emotion in speech synthesis is set to %s" emotion))
    (message "Emotion in speech synthesis cannot be specified.")))

;;;; API

(defun* speech-synth-execute-synthesis (text &key wav-file)
  "Text-to-Speech API for Emacs."
  (lexical-let* ((lang (speech-synth-get-language text))
                 (speech-synth-temp-wav-file
                  (speech-synth-get-temporary-wav-file))
                 (speech-synth-wav-file wav-file)
                 (speech-synth-command
                  (speech-synth-get-command-line lang speech-synth-temp-wav-file)))
    (deferred:$
      ;; speech synthesis
      (deferred:process "sh" "-c"
        (concat "echo " text " | " speech-synth-command))

      ;; play synthesized waveform
      (deferred:nextc it
        (lambda ()
          (cond (speech-synth-play-wav
                 (deferred:process "play" "-q" "--norm" speech-synth-temp-wav-file)))))

      ;; post processing (delete or rename)
      (deferred:nextc it
        (lambda ()
          (cond ((eq speech-synth-wav-file nil)
                 (deferred:process "rm" speech-synth-temp-wav-file))
                (t
                 (deferred:process "mv" speech-synth-temp-wav-file
                   speech-synth-wav-file))))))))

;;;; External Functions

;;;###autoload
(defun speech-synth-from-buffer ()
  "Synthesize speech from strings in buffer."
  (interactive)
  (speech-synth-execute-synthesis
   (speech-synth-get-string (point-min) (point-max))))

;;;###autoload
(defun speech-synth-from-region ()
  "Synthesize speech from strings in region."
  (interactive)
  (speech-synth-execute-synthesis
   (if (use-region-p)
       (speech-synth-get-string (region-beginning) (region-end))
     (error "Region is not active."))))

;;;###autoload
(defun speech-synth (&optional arg)
  "Generate *Speech Synth* buffer to execute speech synthesis."
  (interactive "P")
  (assert speech-synth-buffer-name)
  (let ((buf (cond ((numberp arg)
                    (get-buffer-create (format "%s<%d>"
                                               speech-synth-buffer-name arg)))
                   (arg
                    (generate-new-buffer speech-synth-buffer-name))
                   (t
                    (get-buffer-create speech-synth-buffer-name)))))
    (assert (and buf (buffer-live-p buf)))
    (pop-to-buffer buf)
    (unless (eq major-mode 'speech-synth-mode)
      (speech-synth-mode))
    buf))

(provide 'speech-synth)

;;; speech-synthe.el ends here
