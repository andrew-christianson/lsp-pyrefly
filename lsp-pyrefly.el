;;; lsp-pyrefly.el --- summary -*- lexical-binding: t -*-

;; Author: Andrew Christianson
;; Maintainer: Andrew Christianson
;; Version: 0.0.1
;; Package-Requires: (lsp)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'lsp-mode)

(defgroup lsp-pyrefly nil
  ""
  :group 'lsp-mode)

(defcustom lsp-pyrefly-use-uv t
  "Prefer to start with uv instead of direct"
  :type 'boolean
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-indexing-mode "lazy-non-blocking-background"
  ""
  :type '(choice (const "none")
                 (const "lazy-non-blocking-background")
                 (const "lazy-blocking"))
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-threads 0
  ""
  :type 'integer
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-color "auto"
  ""
  :type '(choice (const "auto")
                 (const "always")
                 (const "never"))
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-workspace-indexing-limit 2000
  ""
  :type 'integer
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-verbose nil
  ""
  :type 'boolean
  :group 'lsp-pyrefly)

(defcustom lsp-pyrefly-extra-command-args '()
  ""
  :type '(repeat string)
  :group 'lsp-pyrefly)

(defun lsp-pyrefly--command-args ()
  (let ((args (list "--indexing-mode" lsp-pyrefly-indexing-mode
                    "--threads" (number-to-string lsp-pyrefly-threads)
                    "--color" lsp-pyrefly-color
                    "--workspace-indexing-limit" (number-to-string lsp-pyrefly-workspace-indexing-limit))))
    (when lsp-pyrefly-verbose
      (setq args (cons "--verbose" args)))
    (append lsp-pyrefly-extra-command-args args)))

(defun lsp-pyrefly--new-connection-args (&optional path)

  (let ((installed (executable-find "pyrefly"))
		(uv (executable-find "uv"))
		(args (cons "lsp" (lsp-pyrefly--command-args)))
		)
	(cond

	 ((and uv lsp-pyrefly-use-uv) (append (list uv "tool" "run" "pyrefly") args))
	 ((and installed) (append (list installed) args))
	 (t (error "can't start pyrefly")))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lsp-pyrefly--new-connection-args))

  :major-modes '(python-mode python-ts-mode)
  :server-id 'pyrefly
  :multi-root nil
  :priority 3))

(provide 'lsp-pyrefly)

;;; lsp-pyrefly.el ends here
