;;; speech-synth.el --- Text-to-Speech API for Emacs.

;; Copyright (C) 2013 by Akira Tamamori

;; Author: Akira TAMAMORI
;; Version: 1.0.0
;; Package-Requires: ((deferred "20130312.1514"))

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
;;     Language (English or Japanese) is automatically selected.
;;
;; `speech-synth-english-from-buffer'
;;     Synthesize English speech from strings in buffer.
;;
;; `speech-synth-japanese-from-buffer'
;;     Synthesize Japanese speech from strings in buffer.
;;
;; `speech-synth-from-buffer'
;;     Synthesize speech from strings in buffer.
;;     Language (English or Japanese) is automatically selected.

;;; Samples:

;;; ChangeLog:
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

(defcustom speech-synth-Flite-voice-file
  "/usr/local/share/cmu_us_arctic_slt.htsvoice"
  "HTS voice file for Flite+hts_engine."
  :type '(file :must-match t)
  :group 'speech-synth)

(defcustom speech-synth-OpenJTalk-voice-file
  "/usr/local/share/nitech_jp_atr503_m001.htsvoice"
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

;;;; Internal variables

(defvar speech-synth-mode-name "Synth"
  "Major mode name displayed in mode line.")

(defvar speech-synth-buffer "*Speech Synth*"
  "Buffer name for speech-synth-mode.")

(defvar speech-synth-spectral-warping 0.55
  "Default value of spectral warping parameter.")

(defvar speech-synth-speaking-rate 1.0
  "Defalut value of speaking rate.")

(defvar speech-synth-pitch-shift 0.0
  "Default value of pitch shift.")

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

(defun speech-synth-get-command-line (lang wav warp pitch rate)
  (cond ((equal lang "English")
         (unless speech-synth-Flite-command
           (error "You must install Flite+hts_engine!"))
         (format "%s -o %s -m %s -a %f -r %f -fm %f"
                 speech-synth-Flite-command
                 wav speech-synth-Flite-voice-file
                 warp rate pitch))
        ((equal lang "Japanese")
         (unless speech-synth-OpenJTalk-command
           (error "You must install Open JTalk!"))
         (format "%s -x %s -ow %s -m %s -a %f -r %f -fm %f"
                 speech-synth-OpenJTalk-command
                 speech-synth-dictionary-directory
                 wav speech-synth-OpenJTalk-voice-file
                 warp rate pitch))
        (t
         (error "You must specify either English or Japanese!"))))

(defun speech-synth-get-parameter ()
  (list (read-number "Spectral transformation: "
                     speech-synth-spectral-warping)
        (read-number "Pitch-shift: "
                     speech-synth-pitch-shift)
        (read-number "Speech rate: "
                     speech-synth-speaking-rate)))

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

;;;; API

(defun* speech-synth-execute-synthesis (lang
                                        text
                                        &key
                                        (warp speech-synth-spectral-warping)
                                        (pitch speech-synth-pitch-shift)
                                        (rate speech-synth-speaking-rate)
                                        wav-file)
  "Text-to-Speech API for Emacs."
  (lexical-let* ((speech-synth-temp-wav-file
                  (speech-synth-get-temporary-wav-file))
                 (speech-synth-wav-file wav-file)
                 (speech-synth-command
                  (speech-synth-get-command-line
                   lang speech-synth-temp-wav-file warp pitch rate)))
    (deferred:$
      ;; speech synthesis
      (deferred:process "sh" "-c"
        (concat "echo " text " | " speech-synth-command))

      ;; play synthesized waveform.
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
(defun speech-synth-english-from-buffer (warp pitch rate)
  "Synthesize English speech from strings in buffer."
  (interactive (speech-synth-get-parameter))
  (let ((str (speech-synth-get-string (point-min) (point-max))))
    (speech-synth-execute-synthesis
     "English" str :warp warp :pitch pitch :rate rate)))

;;;###autoload
(defun speech-synth-japanese-from-buffer (warp pitch rate)
  "Synthesize Japanese speech from strings in buffer."
  (interactive (speech-synth-get-parameter))
  (let ((str (speech-synth-get-string (point-min) (point-max))))
    (speech-synth-execute-synthesis
     "Japanese" str :warp warp :pitch pitch :rate rate)))

;;;###autoload
(defun speech-synth-from-buffer (warp pitch rate)
  "Synthesize speech from strings in buffer."
  (interactive (speech-synth-get-parameter))
  (let* ((str (speech-synth-get-string (point-min) (point-max)))
         (lang (speech-synth-get-language str)))
    (speech-synth-execute-synthesis
     lang str :warp warp :pitch pitch :rate rate)))

;;;###autoload
(defun speech-synth-english-from-region (warp pitch rate)
  (interactive (speech-synth-get-parameter))
  "Synthesize English speech from strings in region."
  (let ((str (speech-synth-get-string (region-beginning) (region-end))))
    (speech-synth-execute-synthesis
     "English" str :warp warp :pitch pitch :rate rate)))

;;;###autoload
(defun speech-synth-japanese-from-region (warp pitch rate)
  (interactive (speech-synth-get-parameter))
  "Synthesize Japanese speech from strings in region."
  (let ((str (speech-synth-get-string (region-beginning) (region-end))))
    (speech-synth-execute-synthesis
     "Japanese" str :warp warp :pitch pitch :rate rate)))

;;;###autoload
(defun speech-synth-from-region (warp pitch rate)
  "Synthesize speech from strings in region."
  (interactive (speech-synth-get-parameter))
  (let* ((str (speech-synth-get-string (region-beginning) (region-end)))
         (lang (speech-synth-get-language str)))
    (speech-synth-execute-synthesis
     lang str :warp warp :pitch pitch :rate rate)))

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
