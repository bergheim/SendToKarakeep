;; -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Summer Emacs <summeremacs@summerstar.me>
;; Maintainer: Summer Emacs <summeremacs@summerstar.me>
;; URL: https://github.com/summeremacs/SendToKarakeep/
;; Version: 1.0.0
;; Package-Requires: Emacs and Karakeep

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
;; You should check outGNU General Public License
;; See <https://www.gnu.org/licenses/>.

;; This is my first code release so I'm not going to spend a lot
;; of time on licenses and stuff like that. Use it and be happy.
;; Just don't use it to blow up the world. If you use it to blow
;; up the world, I'll be most put out! And don't use it to hurt people.


;; Send Link to Karakeep

;; This code simply lets you send an org-mode link or marked text
;; to your karakeep server. You will need to put in your
;; server IP/domain, your port, and the API key that you create
;; in Karakeep.


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
  (if-let* ((link-data (karakeep--get-org-link-at-point))
            (url (car link-data))
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
         (let* ((response (string-trim (buffer-substring-no-properties (point) (point-max))))
                (parsed-response (ignore-errors (json-parse-string response)))
                (response-url (when parsed-response
                                (gethash "url" (gethash "content" parsed-response))))
                (already-exists (when parsed-response
                                  (eq (gethash "alreadyExists" parsed-response) t))))
           (cond
            ((and parsed-response (eq (gethash "alreadyExists" parsed-response) t))
             (message "ℹ️ Link already exists in Karakeep: %s" (or response-url url)))
            ((and parsed-response (gethash "id" parsed-response))
             (message "✅ Link sent to Karakeep: %s" (or response-url url)))
            (t
             (message "❌ Karakeep error: %s" (or response "Unknown error"))))
           (kill-buffer (current-buffer)))))
    (message "⚠️ No valid Org link at point.")))

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
             (if (string-match-p "error" response)
                 (message "❌ Karakeep error: %s" response)
               (message "✅ Text sent to Karakeep.")))
           (kill-buffer (current-buffer)))))
    (message "⚠️ No active region (marked text).")))


(defun elfeed-star-and-send-to-karakeep ()
  "Star the current Elfeed entry and send it to Karakeep.
Works in both elfeed-search-mode and elfeed-show-mode."
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :single)))
         (url (when entry (elfeed-entry-link entry)))
         (title (when entry (elfeed-entry-title entry))))

    (if (not entry)
        (message "⚠️ No entry selected.")
      ;; Add star tag to the entry
      (elfeed-tag entry 'star)
      (when (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update-entry entry))

      ;; Send to Karakeep
      (let* ((json-payload (json-encode
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
           (let* ((response (string-trim (buffer-substring-no-properties (point) (point-max))))
                  (parsed-response (ignore-errors (json-parse-string response)))
                  (response-url (when parsed-response
                                  (gethash "url" (gethash "content" parsed-response))))
                  (already-exists (when parsed-response
                                    (eq (gethash "alreadyExists" parsed-response) t))))
             (cond
              ((and parsed-response (eq (gethash "alreadyExists" parsed-response) t))
               (message "ℹ️ Link already exists in Karakeep: %s" (or response-url url)))
              ((and parsed-response (gethash "id" parsed-response))
               (message "✅ Link starred and sent to Karakeep: %s" (or response-url url)))
              (t
               (message "❌ Karakeep error: %s" (or response "Unknown error"))))
             (kill-buffer (current-buffer)))))))))

(provide 'karakeep-send)
