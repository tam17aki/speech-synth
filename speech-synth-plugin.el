;;; speech-synth-plugin.el --- Plugins for `speech-synth.el'.

;; Copyright (C) 2013 by Akira Tamamori

;; Author: Akira TAMAMORI
;; Version: 0.1

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
;; This package provides plugins for `speech-synth.el'.

;;; Code:

(require 'speech-synth)

;; Mew
;; http://www.mew.org/en, http://www.mew.org/ja

(when (locate-library "mew")
  (eval-after-load "mew"
    '(progn
       (defun mew-biff-bark (n)
         (if (= n 0)
             (setq mew-biff-string nil)
           (if (and mew-use-biff-bell (eq mew-biff-string nil))
               (speech-synth-execute-synthesis
                (format "メールが%d件到着しました．" n)))
           (setq mew-biff-string (format "Mail(%d)" n)))))))

;; Twittering-mode
;; http://twmode.sourceforge.net/index.html

(when (locate-library "twittering-mode")
  (eval-after-load "twittering-mode"
    '(progn
       (defun speech-synth-new-tweets ()
         (when (not (eq (length (twittering-current-timeline-data
                                 twittering-new-tweets-spec))
                        twittering-new-tweets-count))
           (let* ((buf-name (buffer-name (current-buffer)))
                  (num_new_tweets (+ (or (cadr (assq (get-buffer buf-name)
                                                     twittering-unread-status-info)) 0)
                                     (length twittering-rendered-new-tweets))))
             (when (>= num_new_tweets 1)
               (cond ((string= ":home" buf-name)
                      (speech-synth-execute-synthesis
                       (format "新着ツイートが%d件来ましたよ．" num_new_tweets))))))))

       (add-hook 'twittering-new-tweets-rendered-hook
                 #'(lambda () (speech-synth-new-tweets))))))

(provide 'speech-synth-plugin)
