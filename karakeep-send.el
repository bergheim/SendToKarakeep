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

(defgroup karakeep nil
  "Send bookmarks to Karakeep."
  :group 'external)

(defcustom karakeep-api-url "http://localhost:3000/api/v1/bookmarks"
  "Karakeep API endpoint for saving links."
  :type 'string
  :group 'karakeep)

(defcustom karakeep-api-token "YourApiKey"
  "Authorization token for Karakeep."
  :type 'string
  :group 'karakeep)

(defun karakeep--send-request (payload)
  "Send PAYLOAD to Karakeep API.
PAYLOAD should be an alist that will be JSON-encoded."
  (let* ((json-string (json-encode payload))
         (json-payload (string-as-unibyte
                        (encode-coding-string json-string 'utf-8)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " karakeep-api-token))))
         (url-request-data json-payload))
    (url-retrieve karakeep-api-url #'karakeep--handle-response)))

(defun karakeep--handle-response (status)
  "Handle the response from Karakeep API.
STATUS is the response status from url-retrieve."
  (goto-char url-http-end-of-headers)
  (let* ((response (string-trim (buffer-substring-no-properties (point) (point-max))))
         (parsed-response (ignore-errors (json-parse-string response))))
    (cond
     ((and parsed-response (eq (gethash "alreadyExists" parsed-response) t))
      (let ((url (gethash "url" (gethash "content" parsed-response))))
        (message "ℹ️ Already exists in Karakeep%s"
                 (if url (format ": %s" url) ""))))
     ((and parsed-response (gethash "id" parsed-response))
      (let ((url (gethash "url" (gethash "content" parsed-response))))
        (message "✅ Sent to Karakeep%s"
                 (if url (format ": %s" url) ""))))
     (t
      (message "❌ Karakeep error: %s" (or response "Unknown error"))))
    (kill-buffer (current-buffer))))

(defun karakeep--get-org-link-at-point ()
  "Extract the Org-mode link at point."
  (let ((context (org-element-context)))
    (when (eq (car context) 'link)
      (let ((url (org-element-property :raw-link context))
            (desc (org-element-contents context)))
        (list url (if desc (org-trim (org-no-properties (car desc))) ""))))))

(defun karakeep--get-link-at-point ()
  "Extract link at point from various contexts."
  (cond
   ;; either org..
   ((and (derived-mode-p 'org-mode)
         (karakeep--get-org-link-at-point)))

   ;; ..or whatever URL we can find
   ((thing-at-point 'url)
    (list (thing-at-point 'url) ""))))

;; Send link that pointer is over
(defun karakeep-send-link ()
  "Send the link at point to Karakeep."
  (interactive)
  (if-let* ((link-data (karakeep--get-link-at-point))
            (url (car link-data))
            (title (cadr link-data)))
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title)))
    (message "⚠️ No valid link at point.")))

(defun karakeep-send-region ()
  "Send the currently active region (marked text) to Karakeep as a 'text' type."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (karakeep--send-request `(("type" . "text")
                                  ("text" . ,text))))
    (message "⚠️ No active region (marked text).")))


(defun karakeep-send-elfeed-entry ()
  "Star the current Elfeed entry and send it to Karakeep."
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
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))))))

(defun karakeep-send-eww-page ()
  "Send the current EWW page to Karakeep."
  (interactive)
  (let* ((url (eww-current-url))
         (title (plist-get eww-data :title)))
    (when url
      (karakeep--send-request `(("type" . "link")
                                ("url" . ,url)
                                ("title" . ,title))))))

;;;###autoload
(defun karakeep-dwim ()
  "Send content to Karakeep based on context.
- In elfeed: send current entry
- In eww: send current page url
- With active region: send selected text
- A link: send the link (org or otherwise)."
  (interactive)
  (cond
   ;; Elfeed context
   ((or (eq major-mode 'elfeed-show-mode)
        (eq major-mode 'elfeed-search-mode))
    (karakeep-send-elfeed-entry))

   ((eq major-mode 'eww-mode)
    (karakeep-send-eww-page))

   ((use-region-p)
    (karakeep-send-region))

   ;; Links - should probably not be the fallback but good enough for now
   (t
    (karakeep-send-link))))

(provide 'karakeep-send)
