;;; channels.scm — pin Guix to a 2024 snapshot where Mesa is 24.0.4.
;;;
;;; Bitwig 5.2.7 segfaults inside libvulkan_intel.so on current Guix's Mesa
;;; (25.x).  The flake.nix in this directory targets nixpkgs-24.05, which
;;; ships Mesa 24.1.x.  Guix never packaged 24.1, so we use the last
;;; pre-24.2 commit (Mesa 24.0.4, 2024-11-05) — close enough.
;;;
;;; Usage:
;;;
;;;   # One-shot run:
;;;   guix time-machine -C channels.scm -- \
;;;     shell -f guix.scm -- bitwig-studio-wrapped
;;;
;;;   # Install into profile so bitwig.desktop's Exec= can be a plain path:
;;;   guix time-machine -C channels.scm -- package -f guix.scm
;;;
;;; The first time-machine invocation builds/downloads the pinned Guix and
;;; caches it under ~/.cache/guix; subsequent runs are fast.

(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit "4c56d0cccdc44e12484b26332715f54768738c5f")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
