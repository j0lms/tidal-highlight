;;; tidal-relay.el --- Native module loader for Tidal Highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is responsible for loading the native Rust module (`tidal_relay`)
;; that facilitates real-time OSC event communication for TidalCycles
;; highlighting in Emacs.

;;; Code:

(require 'cl-lib)

(defconst tidal-relay--dyn-name "tidal_relay"
  "Dynamic module name.")

(defconst tidal-relay--bin-dir
  (concat
   (file-name-directory (or load-file-name buffer-file-name)) "bin/")
  "Pre-built binaries directory path.")

(defvar tidal-relay-module-loaded nil
  "Tracks whether the module has been successfully loaded.")

(defun tidal-relay-load-module ()
  "Load the native tidal-relay module."
  (interactive)
  (when (not tidal-relay-module-loaded)
    (let* ((dyn-lib-path
            (expand-file-name
             (cl-case
              system-type
              (windows-nt
               (concat "windows/" tidal-relay--dyn-name ".dll"))
              (gnu/linux
               (concat "linux/" tidal-relay--dyn-name ".so"))
              (t
               (error
                "Tidal Relay: Unsupported operating system: %s"
                system-type)))
             tidal-relay--bin-dir))
           (dyn-lib-name (file-name-nondirectory dyn-lib-path)))
      (if (not (file-exists-p dyn-lib-path))
          (error "Tidal Relay: Module not found at %s" dyn-lib-path)
        (condition-case err
            (progn
              (module-load dyn-lib-path)
              (setq tidal-relay-module-loaded t)
              (message
               "Tidal Relay: Successfully loaded dynamic module: %s"
               dyn-lib-name))
          (error
           (error "Tidal Relay: Critical Load Error: %s" err)))))))

(provide 'tidal-relay)
;;; tidal-relay.el ends here
