;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote
;; Version: 4.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Send Link to Karakeep

;; This code simply lets you send an org-mode link or marked text to your karakeep server. You will need to put in your server IP/domain, your port, and the API key that you create in Karakeep.


(defvar karakeep-api-url "http://YourIPorAddress:YourPort/api/v1/bookmarks"
  "Karakeep API endpoint for saving links.")

(defvar karakeep-api-token "YourAPIKey"
  "Authorization token for Karakeep.")

(defun karakeep--get-org-link-at-point ()
  "Extract the Org-mode link at point."
  (let ((context (org-element-context)))
    (when (eq (car context) 'link)
      (let ((url (org-element-property :raw-link context))
            (desc (org-element-contents context)))
        (list url (if desc (org-trim (org-no-properties (car desc))) ""))))))

;; Send link that pointer is over
(defun karakeep-send-link ()
  "Send the Org link at point to Karakeep."
  (interactive)
  (let* ((link-data (karakeep--get-org-link-at-point)))
    (if link-data
        (let* ((url (car link-data))
               (title (cadr link-data))
               (json-payload (json-encode
                              `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))))
               (url-request-method "POST")
               (url-request-extra-headers
                `(("Content-Type" . "application/json")
                  ("Authorization" . ,(concat "Bearer " karakeep-api-token))))
               (url-request-data json-payload))
          (url-retrieve
           karakeep-api-url
           (lambda (status)
             (goto-char url-http-end-of-headers)
             (let ((response (buffer-substring-no-properties (point) (point-max))))
               (message "Response: %s" response)
               (if (string-match-p "error" response)
                   (message "❌ Karakeep error: %s" response)
                 (message "✅ Link sent to Karakeep: %s" url)))
             (kill-buffer (current-buffer)))))
      (message "⚠️ No valid Org link at point."))))

;; Send marked text
(defun karakeep-send-marked-text ()
  "Send the currently active region (marked text) to Karakeep as a 'text' type."
  (interactive)
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (json-payload (json-encode
                            `(("type" . "text")
                              ("text" . ,text))))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " karakeep-api-token))))
             (url-request-data json-payload))
        (url-retrieve
         karakeep-api-url
         (lambda (status)
           (goto-char url-http-end-of-headers)
           (let ((response (buffer-substring-no-properties (point) (point-max))))
             (message "Response: %s" response)
             (if (string-match-p "error" response)
                 (message "❌ Karakeep error: %s" response)
               (message "✅ Text sent to Karakeep.")))
           (kill-buffer (current-buffer)))))
    (message "⚠️ No active region (marked text).")))

(provide 'karakeep-send)

