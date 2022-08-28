;;; relative-date.el --- Relative dates computation & formating -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/relative-date
;; Keywords: convenience
;; Version: 0.2.1

;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows to format the difference between two dates according to
;; the value of their difference (expressed in seconds) or a symbolic
;; relationship between the two dates (e.g. 'today). The main entry points for
;; the library are the relative-date function and the relative-date-formats.
;;
;;; News
;;
;; Version 0.1
;; - Initial release
;;

;;; Code
(defcustom relative-date-formats
  `( (,(*       3 60) . "now")              ;; Less than 3 minutes (past)
     (,(- (*   3 60)) . "soon")             ;; Less than 3 minutes (future)
     (,(*      60 60) . "%(M) mins. ago")   ;; Less than 1 hour
     (,(*    3 60 60) . "%(H) hours ago")   ;; Less than 3 hours
     (today           . "Today %H:%M")      ;; Today
     (yesterday       . "Yest. %H:%M")      ;; Yesterday
     (tomorrow        . "Tom. %H:%M")       ;; Tomorrow
     (this-week       . "%a. %H:%M")        ;; This week
     (this-year       . "%B %d")            ;; This year
     (t               . "%Y-%m-%d"))       ;; Default
  
  "Ordered list of (criterion . format) for formating a date difference.
A numerical criterion means that the date difference expressed in seconds has to be less than the given value. A symbolic criterion means to check for its validity. The format variable can contain the common date formatter and it is complemented by relative difference expressed in minutes %(M), hours %(H), days %(d), weeks %(w), months %(m) and years %(y)."

  :type '(repeat (cons (radio :tag "Criterion"
                               (number :tag "Numerical (seconds)")
                               (choice :tag "Symbolic"
                                       (const yesterday)
                                       (const today)
                                       (const tomorrow)
                                       (const last-week)
                                       (const this-week)
                                       (const next-week)
                                       (const last-month)
                                       (const this-month)
                                       (const next-month)
                                       (const last-year)
                                       (const this-year)
                                       (const next-year))
                               (boolean :tag "Default"))
                     (string :tag "Format"))))
  

(defun relative-date-day (date)
  "Return DATE day of month (1-31)."

  (nth 3 (decode-time date)))

(defun relative-date-month (date)
  "Return DATE month number (1-12)."
  
  (nth 4 (decode-time date)))

(defun relative-date-year (date)
  "Return DATE year."

  (nth 5 (decode-time date)))

(defun relative-date-equal (date1 date2)
  "Check if DATE1 is equal to DATE2."
  
  (and (eq (relative-date-day date1)
           (relative-date-day date2))
       (eq (relative-date-month date1)
           (relative-date-month date2))
       (eq (relative-date-year date1)
           (relative-date-year date2))))

(defun relative-date-inc (date &optional days months years)
  "Return DATE + DAYS day + MONTH months + YEARS years"

  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0))
        (day (relative-date-day date))
        (month (relative-date-month date))
        (year (relative-date-year date)))
    (encode-time 0 0 0 (+ day days) (+ month months) (+ year years))))

(defun relative-date-dec (date &optional days months years)
  "Return DATE - DAYS day - MONTH months - YEARS years"
  
  (let ((days (or days 0))
        (months (or months 0))
        (years (or years 0)))
    (relative-date-inc date (- days) (- months) (- years))))

(defun relative-date-today (&optional now)
  "Return today date."

  (or now (current-time)))

(defun relative-date-is-today (date &optional now)
  "Check if DATE is today."

  (let ((now (or now (current-time))))
    (relative-date-equal now date)))

(defun relative-date-is-yesterday (date &optional now)
  "Check if DATE is yesterday."

  (let ((now (or now (current-time))))
    (relative-date-equal (relative-date-dec (relative-date-today now) 1) date)))

(defun relative-date-is-tomorrow (date &optional now)
  "Check if DATE is tomorrow."

  (let ((now (or now (current-time))))
    (relative-date-equal (relative-date-inc (relative-date-today now) 1) date)))

(defun relative-date-is-this-week (date &optional now)
  "Check if DATE corresponds to current week (week starts on Monday)"

  (let* ((now (or now (current-time)))
         (delta (float-time (time-subtract now date)))
         (delta (ceiling (/ delta (*   24 60 60))))
         (week (mod (string-to-number (format-time-string "%W" date)) 52))
         (this-week (mod (string-to-number (format-time-string "%W" now)) 52)))
    (and (>= delta 0) (< delta 7) (= week this-week))))

(defun relative-date-is-last-week (date &optional now)
  "Check if date corresponds to last week (week starts on Monday)"

  (let* ((now (or now (current-time)))
         (delta (float-time (time-subtract now date)))
         (delta (ceiling (/ delta (*   24 60 60))))
         (week (mod (string-to-number (format-time-string "%W" date)) 52))
         (last-week (mod (- (string-to-number (format-time-string "%W" now)) 1) 52)))
    (and (>= delta 0) (< delta 14) (= week last-week))))

(defun relative-date-is-next-week (date &optional now)
  "Check if date corresponds to last week (week starts on Monday)"

  (let* ((now (or now (current-time)))
         (delta (float-time (time-subtract now date)))
         (delta (ceiling (/ delta (*   24 60 60))))
         (week (mod (string-to-number (format-time-string "%W" date)) 52))
         (next-week (mod (+ (string-to-number (format-time-string "%W" now)) 1) 52)))
    (and (>= delta 0) (< (abs delta) 14) (= week next-week))))

(defun relative-date-is-this-month (date &optional now)
  "Check if DATE corresponds to current month"

  (let* ((now (or now (current-time))))
    (and (= (relative-date-year now) (relative-date-year date))
         (= (relative-date-month now) (relative-date-month date)))))

(defun relative-date-is-last-month (date &optional now)
  "Check if DATE corresponds to last month"

  (let* ((now (or now (current-time))))
    (or
     ;; Same year
     (and (= (relative-date-year date) (relative-date-year now))
          (= (relative-date-month date) (- (relative-date-month now) 1)))
     ;; Previous year
     (and (= (relative-date-year date) (- (relative-date-year now) 1) )
          (= (relative-date-month date) 12)
          (= (relative-date-month now) 1)))))

(defun relative-date-is-next-month (date &optional now)
  "Check if DATE corresponds to last month"

  (let* ((now (or now (current-time))))
    (or
     ;; Same year
     (and (= (relative-date-year date) (relative-date-year now))
          (= (relative-date-month date) (+ (relative-date-month now) 1)))
     ;; Next year
     (and (= (relative-date-year date) (+ (relative-date-year now) 1) )
          (= (relative-date-month date) 1)
          (= (relative-date-month now) 12)))))

(defun relative-date-is-this-year (date &optional now)
  "Check if DATE corresponds to current year."

  (let* ((now (or now (current-time))))
    (= (relative-date-year now) (relative-date-year date))))

(defun relative-date-is-last-year (date &optional now)
  "Check if DATE corresponds to last year."

  (let* ((now (or now (current-time))))
    (= (relative-date-year date) (- (relative-date-year now) 1))))

(defun relative-date-is-next-year (date &optional now)
  "Check if DATE corresponds to next year."

  (let* ((now (or now (current-time))))
    (= (relative-date-year date) (+ (relative-date-year now) 1))))

(defun relative-date (date &optional now)
  "Return the relative difference between DATE and NOW according to RELATIVE-DATE-FORMATS."

  (let* ((now (or now (current-time)))
         (delta (float-time (time-subtract now date)))
         
         (delta-minute (ceiling (/ delta (*          60))))
         (delta-hour   (ceiling (/ delta (*       60 60))))
         (delta-day    (ceiling (/ delta (*    24 60 60))))
         (delta-week   (ceiling (/ delta (*  7 24 60 60))))
         (delta-month  (ceiling (/ delta (* 30 24 60 60))))
         (delta-year   (- (relative-date-year now)  (relative-date-year date)))
         (symbols `((today      . ,(relative-date-is-today date now))
                    (yesterday  . ,(relative-date-is-yesterday date now))
                    (tomorrow   . ,(relative-date-is-tomorrow date now))
                    (last-week  . ,(relative-date-is-last-week date now))
                    (this-week  . ,(relative-date-is-this-week date now))
                    (next-week  . ,(relative-date-is-next-week date now))
                    (last-month . ,(relative-date-is-last-month date now))
                    (this-month . ,(relative-date-is-this-month date now))
                    (next-month . ,(relative-date-is-next-month date now))
                    (last-year  . ,(relative-date-is-last-year date now))
                    (this-year  . ,(relative-date-is-this-year date now))
                    (next-year  . ,(relative-date-is-this-year date now))))
         (result (format-time-string "%Y-%m-%d" date)))
    
      (catch 'found
        (dolist (item relative-date-formats)
          (let ((value (car item))
                (format (cdr item)))
            (when (or (and (numberp value) (>= value 0) (>= delta 0) (< delta value))
                      (and (numberp value) (<= value 0) (<= delta 0) (< value delta))
                      (and (symbolp value) (cdr (assoc value symbols))))
              (let* ((format (string-replace "%(M)" (format "%d" delta-minute) format))
                     (format (string-replace "%(H)" (format "%d" delta-hour) format))
                     (format (string-replace "%(d)" (format "%d" delta-day) format))
                     (format (string-replace "%(w)" (format "%d" delta-week) format))
                     (format (string-replace "%(m)" (format "%d" delta-month) format))
                     (format (string-replace "%(y)" (format "%d" delta-year) format)))
                (setq result (format-time-string format date)))
              (throw 'found result)))))
      result))


(provide 'relative-date)
;;; relative-date.el ends here
