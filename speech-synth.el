;;; speech-synth.el --- Text-to-Speech API for Emacs.

;; Copyright (C) 2013 by Akira Tamamori

;; Author: Akira TAMAMORI
;; Version: 1.1.0
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
;; This package provides a text-to-speech API for Emacs.
;; Using Flite+hts_engine and Open JTalk, speech synthesis of English and
;; Japanese can be performed. Therefore, you must install hts_engine-API,
;; Flite+hts_engine and Open JTalk in advance.

;;; Installation:
;;
;; Please check version numbers of the following installed softwares:
;; hts_engine-API >= 1.07, Flite+hts_engine >= 1.04 and Open JTalk >= 1.06.
;;
;; deferred.el is required for asynchronous process:
;; [http://github.com/kiwanami/emacs-deferred/blob/master/deferred.el].
;;
;; sox is also required to play synthesized waveform:
;; [http://sox.sourceforge.net].
;;
;; Next, put speech-synth.el in your load-path and add followings
;; to your `user-init-file':
;;
;; ----------------------------------------------------------------------------
;; (require 'speech-synth)
;; ----------------------------------------------------------------------------
;;
;; Also, you must specify correct path names of them in accordance with your
;; environment:
;;
;; ----------------------------------------------------------------------------
;; (setq speech-synth-dictionary-directory "/usr/local/share/dic")
;; (setq speech-synth-Flite-voice-file
;;       "/usr/local/share/hts_engine/cmu_us_arctic_slt.htsvoice")
;; (setq speech-synth-OpenJTalk-voice-file
;;       "/usr/local/share/hts_engine/nitech_jp_atr503_m001.htsvoice")
;; ----------------------------------------------------------------------------

;;; Commands:
;;
;; `speech-synth'
;;     Generate *Speech Synth* buffer to input text for speech synthesis.
;;
;; `speech-synth-english-from-region'
;;     Synthesize English speech from strings in region.
;;
;; `speech-synth-japanese-from-region'
;;     Synthesize Japanese speech from strings in region.
;;
;; `speech-synth-from-region'
;;     Synthesize speech from strings in region.
;;     When `speech-synth-auto-select-language' is t,
;;     language (English or Japanese) in speech synthesis is automatically selected.
;;
;; `speech-synth-english-from-buffer'
;;     Synthesize English speech from strings in buffer.
;;
;; `speech-synth-japanese-from-buffer'
;;     Synthesize Japanese speech from strings in buffer.
;;
;; `speech-synth-from-buffer'
;;     Synthesize speech from strings in buffer.
;;     When `speech-synth-auto-select-language' is t,
;;     language (English or Japanese) in speech synthesis is automatically selected.
;;
;; `speech-synth-set-language'
;;     Specify language in speech synthesis.
;;     Only when `speech-synth-auto-select-language' is nil,
;;     speech is synthesised in the specified language.
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
  "In non-nil, language in speech synthesis is selected automatically."
  :type 'boolean
  :group 'speech-synth)

(defcustom speech-synth-emotional-speech nil
  "If non-nil, emotional speech synthesis can be used (Japanese only).
Of cource, you must prepare HTS voice files for each emotion."
  :type 'boolean
  :group 'speech-synth)

;;;; Internal variables

(defvar speech-synth-mode-name "Synth"
  "Major mode name displayed in mode line.")

(defvar speech-synth-buffer "*Speech Synth*"
  "Buffer name for speech-synth-mode.")

(defvar speech-synth-spectral-warping-default 0.55
  "Default value of spectral warping parameter.")

(defvar speech-synth-speaking-rate-default 1.0
  "Defalut value of speaking rate.")

(defvar speech-synth-pitch-shift-default 0.0
  "Default value of pitch shift.")

(defvar speech-synth-voice-volume-default 1.0
  "Default value of volume of voice.")

(defvar speech-synth-intonation-default 1.0
  "Default value of intonation.")

(defvar speech-synth-postfilter-default 0.0
  "Default value of post-filtering coefficient.")

(defvar speech-synth-emotion-default "normal"
  "Default of emotion in speech synthesis.")

(defvar speech-synth-emotion-prefix "mei_"
  "Prefix of voice file for emotinal speech synthesis.")

(defvar speech-synth-language "English"
  "Default language in speech speech.")

(defvar speech-synth-spectral-warping speech-synth-spectral-warping-default
  "Value of spectral warping parameter.")

(defvar speech-synth-speaking-rate speech-synth-speaking-rate-default
  "Value of speaking rate.")

(defvar speech-synth-pitch-shift speech-synth-pitch-shift-default
  "Value of pitch shift.")

(defvar speech-synth-voice-volume speech-synth-voice-volume-default
  "Value of volume of voice.")

(defvar speech-synth-intonation speech-synth-intonation-default
  "Value of intonation.")

(defvar speech-synth-emotion speech-synth-emotion-default
  "Value of emotion in speech.")

(defvar speech-synth-postfilter speech-synth-postfilter-default
  "Value of postfilter coefficient in speech synthesis.")

(defvar speech-synth-mode-key-table
  '(("C-c C-j"   . speech-synth-japanese-from-buffer)
    ("C-c C-e"   . speech-synth-english-from-buffer)
    ("C-c C-M-j" . speech-synth-japanese-from-region)
    ("C-c C-M-e" . speech-synth-english-from-region)
    ("C-c C-r"   . speech-synth-from-region)
    ("C-c C-b"   . speech-synth-from-buffer)
    ("C-c C-q"   . speech-synth-quit))
  "Default key table for speech-synth-mode.")

(defvar speech-synth-language-list '("English" "Japanese"))

(defvar speech-synth-parameter-list
  '("Spectral warping" "Speech rate" "Pitch-shift" "Voice volume"
    "Intonation" "Postfilter"))

(defvar speech-synth-emotion-list '("normal" "happy" "sad" "angry" "bashful"))

;;;; Major mode

(define-derived-mode speech-synth-mode fundamental-mode speech-synth-mode-name
  "Major mode for speech synthesis."
  :group 'speech-synth
  (loop for (key . cmd) in speech-synth-mode-key-table
        do (define-key speech-synth-mode-map (read-kbd-macro key) cmd)))

;;;; Internal functions

(defun speech-synth-popup ()
  (pop-to-buffer speech-synth-buffer))

(defun speech-synth-quit ()
  (interactive)
  (view-mode 1) (View-quit))

(defun speech-synth-get-string (start end)
  (shell-quote-argument
   (replace-regexp-in-string
    "\n" " " (buffer-substring-no-properties start end))))

(defun speech-synth-get-temporary-wav-file ()
  (concat (make-temp-name
           (expand-file-name temporary-file-directory)) ".wav"))

(defun speech-synth-get-command-line (lang wav)
  (cond ((equal lang "English")
         (unless speech-synth-Flite-command
           (error "You must install Flite+hts_engine!"))
         (format "%s -o %s -m %s -a %f -r %f -fm %f -jm %f -jf %f -b %f"
                 speech-synth-Flite-command
                 wav
                 speech-synth-Flite-voice-file
                 speech-synth-spectral-warping
                 speech-synth-speaking-rate
                 speech-synth-pitch-shift
                 speech-synth-voice-volume
                 speech-synth-intonation
                 speech-synth-postfilter))
        ((equal lang "Japanese")
         (unless speech-synth-OpenJTalk-command
           (error "You must install Open JTalk!"))
         (format "%s -x %s -ow %s -m %s -a %f -r %f -fm %f -jm %f -jf %f -b %f"
                 speech-synth-OpenJTalk-command
                 speech-synth-dictionary-directory
                 wav
                 speech-synth-OpenJTalk-voice-file
                 speech-synth-spectral-warping
                 speech-synth-speaking-rate
                 speech-synth-pitch-shift
                 speech-synth-voice-volume
                 speech-synth-intonation
                 speech-synth-postfilter))
        (t
         (error "You must specify either English or Japanese!"))))

(defun speech-synth-get-parameter (use_default)
  (cond ((eq use_default t)
         (list (read-number "Spectral warping: "
                            speech-synth-spectral-warping-default)
               (read-number "Speech rate: "
                            speech-synth-speaking-rate-default)
               (read-number "Pitch-shift: "
                            speech-synth-pitch-shift-default)
               (read-number "Voice volume: "
                            speech-synth-voice-volume-default)
               (read-number "Intonation: "
                            speech-synth-intonation-default)
               (read-number "Postfilter: "
                            speech-synth-postfilter-default)))
        (t
         (list speech-synth-spectral-warping
               speech-synth-speaking-rate
               speech-synth-pitch-shift
               speech-synth-voice-volume
               speech-synth-intonation
               speech-synth-postfilter))))

(defun speech-synth-set-parameter-reset ()
  (setq speech-synth-spectral-warping speech-synth-spectral-warping-default
        speech-synth-speaking-rate speech-synth-speaking-rate-default
        speech-synth-pitch-shift speech-synth-pitch-shift-default
        speech-synth-voice-volume speech-synth-voice-volume-default
        speech-synth-intonation speech-synth-intonation-default
        speech-synth-postfilter speech-synth-postfilter-default)
  (message "All speech synthesis parameters are reset."))

;;;###autoload
(defun speech-synth-set-parameter-interactive (arg)
  (interactive "P")
  (if arg
      (speech-synth-set-parameter-reset)
    (let ((param (completing-read "Parameter: " speech-synth-parameter-list nil t)))
      (cond ((equal param "Spectral warping")
             (let ((warp (read-number "Spectral warping: "
                                      speech-synth-spectral-warping-default)))
               (when (or (> warp 1.0) (< warp 0.0))
                 (error "Warping parameter must be between 0.0 and 1.0!"))
               (setq speech-synth-spectral-warping warp)))
            ((equal param "Speech rate")
             (let ((rate (read-number "Speech rate: "
                                      speech-synth-speaking-rate-default)))
               (when (< rate 0.0)
                 (error "Speaking rate parameter must be grater than 0.0!"))
               (setq speech-synth-speaking-rate rate)))
            ((equal param "Pitch-shift")
             (let ((pitch (read-number "Pitch-shift: "
                                       speech-synth-pitch-shift-default)))
               (when (or (> pitch 1.0) (< pitch 0.0))
                 (error "Pitch shift parameter must be between 0.0 and 1.0!"))
               (setq speech-synth-pitch-shift pitch)))
            ((equal param "Voice volume")
             (let ((volume (read-number "Voice volume: "
                                        speech-synth-voice-volume-default)))
               (when (< volume 0.0)
                 (error "Voice volume parameter must be greater than 0.0!"))
               (setq speech-synth-voice-volume volume)))
            ((equal param "Intonation")
             (let ((intonation (read-number "Intonation: "
                                            speech-synth-intonation-default)))
               (when (< intonation 0.0)
                 (error "Intonation parameter must be greater than 0.0!"))
               (setq speech-synth-intonation intonation)))
            ((equal param "Postfilter")
             (let ((postfilter (read-number "Postfilter: "
                                            speech-synth-postfilter-default)))
               (when (or (> postfilter 1.0) (< postfilter 0.0))
                 (error "Postfilter parameter must be between 0.0 and 1.0!"))
               (setq speech-synth-postfilter postfilter)))))))

(defun speech-synth-set-parameter (param param_val &optional reset_all)
  (if reset_all
      (speech-synth-set-parameter-reset))
  (cond ((equal param "Spectral warping")
         (when (or (> param_val 1.0) (< param_val 0.0))
           (error "Warping parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-spectral-warping param_val))
        ((equal param "Speech rate")
         (when (or (> param_val 1.0) (< param_val 0.0))
             (error "Pitch shift parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-pitch-shift param_val))
        ((equal param "Pitch-shift")
         (when (< param_val 0.0)
           (error "Speaking rate parameter must be grater than 0.0!"))
         (setq speech-synth-speaking-rate param_val))
        ((equal param "Voice volume")
         (when (< param_val 0.0)
           (error "Voice volume parameter must be greater than 0.0!"))
         (setq speech-synth-voice-volume param_val))
        ((equal param "Intonation")
         (when (< param_val 0.0)
           (error "Intonation parameter must be greater than 0.0!"))
         (setq speech-synth-intonation param_val))
        ((equal param "Postfilter")
         (when (or (> param_val 1.0) (< param_val 0.0))
           (error "Postfilter parameter must be between 0.0 and 1.0!"))
         (setq speech-synth-postfilter param_val))
        (t
         (error "Wrong type parameter is specified!"))))

;;;###autoload
(defun speech-synth-set-emotion ()
  (interactive)
  (if (and speech-synth-emotional-speech (equal speech-synth-language "Japanese"))
      (let ((emotion (completing-read "Emotion: " speech-synth-emotion-list nil t)))
        (setq speech-synth-OpenJTalk-voice-file
              (concat speech-synth-voice-directory speech-synth-emotion-prefix
                      emotion ".htsvoice"))
        (message "Emotion in speech synthesis is set to %s" emotion))
    (message "Emotion in speech synthesis cannot be specified.")))

(defun speech-synth-get-language (text)
  (if current-prefix-arg
      (completing-read "Language: " speech-synth-language-list nil t)
    (cond
     ((> (/ (* (length (replace-regexp-in-string "[^A-Za-z 0-9]+" "" text))
               100)
            (length text))
         speech-synth-lang-select-percent)
      "English")
     (t
      "Japanese"))))

;;;###autoload
(defun speech-synth-set-language ()
  (interactive)
  (let ((lang (completing-read "Language: " speech-synth-language-list nil t)))
    (setq speech-synth-language lang)
    (message "Language in speech synthesis is set to %s" lang)))

;;;; API

(defun* speech-synth-execute-synthesis (lang text &key wav-file)
  "Text-to-Speech API for Emacs."
  (lexical-let* ((speech-synth-temp-wav-file
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
                 (deferred:process "play" "-q" speech-synth-temp-wav-file)))))

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
(defun speech-synth-english-from-buffer ()
  "Synthesize English speech from strings in buffer."
  (interactive)
  (let ((str (speech-synth-get-string (point-min) (point-max))))
    (speech-synth-execute-synthesis "English" str)))

;;;###autoload
(defun speech-synth-japanese-from-buffer ()
  "Synthesize Japanese speech from strings in buffer."
  (interactive)
  (let ((str (speech-synth-get-string (point-min) (point-max))))
    (speech-synth-execute-synthesis "Japanese" str)))

;;;###autoload
(defun speech-synth-from-buffer ()
  "Synthesize speech from strings in buffer."
  (interactive)
  (let* ((str (speech-synth-get-string (point-min) (point-max)))
         (lang (if speech-synth-auto-select-language
                   (speech-synth-get-language str)
                 speech-synth-language)))
    (speech-synth-execute-synthesis lang str)))

;;;###autoload
(defun speech-synth-english-from-region ()
  (interactive)
  "Synthesize English speech from strings in region."
  (let ((str (speech-synth-get-string (region-beginning) (region-end))))
    (speech-synth-execute-synthesis "English" str)))

;;;###autoload
(defun speech-synth-japanese-from-region ()
  (interactive)
  "Synthesize Japanese speech from strings in region."
  (let ((str (speech-synth-get-string (region-beginning) (region-end))))
    (speech-synth-execute-synthesis "Japanese" str)))

;;;###autoload
(defun speech-synth-from-region ()
  "Synthesize speech from strings in region."
  (interactive)
  (let* ((str (speech-synth-get-string (region-beginning) (region-end)))
         (lang (if speech-synth-auto-select-language
                   (speech-synth-get-language str)
                 speech-synth-language)))
    (speech-synth-execute-synthesis lang str)))

;;;###autoload
(defun speech-synth ()
  "Generate *Speech Synth* buffer to execute speech synthesis."
  (interactive)
  (get-buffer-create speech-synth-buffer)
  (with-current-buffer speech-synth-buffer
    (speech-synth-mode))
  (speech-synth-popup))

(provide 'speech-synth)

;;; speech-synthe.el ends here
