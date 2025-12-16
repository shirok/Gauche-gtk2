;;
;; Package Gauche-gtk2
;;

;;
;; NOTE: This isn't currently used, but will eventually effective once
;; we switch autoconf-generated configure to Gauche's native configure
;; (currently configure.scm).
;;

(define-gauche-package "Gauche-gtk2"
  ;; Repository URL, e.g. github
  ;;  This URL uniquely identifies the package.
  :repository "https://github.com/shirok/Gauche-gtk2.git"

  ;;
  :version "0.6.2"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Gtk binding for Gauche"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.16_")))

  ;; List of providing modules
  ;; NB: This will be recognized >= Gauche 0.9.7.
  ;; Example:
  ;;      :providing-modules (util.algorithm1 util.algorithm1.option)
  :providing-modules (gtk)

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Shiro Kawai <shiro@acm.org>"
            "Michal Maruška <mmc@maruska.dyndns.org>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ("Shiro Kawai <shiro@acm.org>")

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("BSD")

  ;; Homepage URL, if any.
  :homepage "https://practical-scheme.net/gauche/"
  )
