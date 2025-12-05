# tidal-highlight

Event highlighting and macros for TidalCycles in Emacs.

## Installation

### elpaca

```emacs-lisp
(use-package tidal)

(use-package tidal-highlight
  :after tidal
  :ensure (tidal-highlight
		   :host github
		   :repo "j0lms/tidal-highlight" 
		   :files ("*.el" "bin"))
  :config
  (setq tidal-highlight-udp-port 6012)) ;; Must match the editorTarget port
```