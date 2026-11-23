;;; guix.scm — Bitwig Studio 5.2.7 with pinned Mesa/Vulkan/X11 libraries.
;;; Guix equivalent of flake.nix in this directory.
;;;
;;; Run Bitwig (ephemeral, like `nix run`):
;;;   guix shell -f guix.scm -- bitwig-studio-wrapped
;;;
;;; Install into your profile so bitwig.desktop can launch it:
;;;   guix package -f guix.scm
;;;   # Exec=...guix-profile/bin/bitwig-studio-wrapped %F
;;;
;;; To refresh the source hash (e.g. on version bump):
;;;   guix download "https://www.bitwig.com/dl/Bitwig%20Studio/<ver>/installer_linux/"

(use-modules (guix packages)
             (guix download)
             (guix gexp)
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages compression)
             (gnu packages gl)
             (gnu packages vulkan)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages debian))

(define bitwig-version "5.2.7")

(define bitwig-source
  (origin
    (method url-fetch)
    (uri (string-append "https://www.bitwig.com/dl/Bitwig%20Studio/"
                        bitwig-version "/installer_linux/"))
    (file-name (string-append "bitwig-studio-" bitwig-version ".deb"))
    (sha256
     (base32 "1hlhc6j48iazp0igxq0x4xvd4xf8z1naxq8ignx9hhski2lvna2g"))))

;; (guix licenses) doesn't export its constructor publicly, so reach in directly.
(define bitwig-license
  ((@@ (guix licenses) license)
   "Bitwig Studio EULA"
   "https://www.bitwig.com/legal/"
   "Proprietary; redistribution restricted."))

(define-public bitwig-studio
  (package
    (name "bitwig-studio")
    (version bitwig-version)
    (source bitwig-source)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          ;; dpkg-deb shells out to `tar` and (for zstd-compressed debs) `zstd`.
          (setenv "PATH"
                  (string-join (list #+(file-append tar "/bin")
                                     #+(file-append zstd "/bin"))
                               ":"))
          (let ((out #$output))
            (mkdir-p out)
            (invoke #+(file-append dpkg "/bin/dpkg-deb")
                    "-x" #$bitwig-source out)
            (rename-file (string-append out "/opt/bitwig-studio")
                         (string-append out "/bitwig-studio"))
            (delete-file-recursively (string-append out "/opt"))))))
    (home-page "https://www.bitwig.com")
    (synopsis "Bitwig Studio 5.2.7 digital audio workstation (binary)")
    (description
     "Pinned binary release of Bitwig Studio.  Use @code{bitwig-studio-wrapped}
to launch it with a known-good set of Mesa/Vulkan/X11 libraries.")
    (license bitwig-license)))

(define runtime-libs
  (list mesa vulkan-loader libglvnd
        libx11 libxcb libxcursor libxrender libxfixes
        libxkbcommon xcb-util-wm))

(define-public bitwig-studio-wrapped
  (package
    (inherit bitwig-studio)
    (name "bitwig-studio-wrapped")
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out #$output)
                 (bin (string-append out "/bin"))
                 (script (string-append bin "/bitwig-studio-wrapped"))
                 (lib-path
                  (string-join
                   (list #$@(map (lambda (p) (file-append p "/lib"))
                                 runtime-libs))
                   ":")))
            (mkdir-p bin)
            (call-with-output-file script
              (lambda (port)
                (format port "#!~a/bin/bash~%" #$bash-minimal)
                (format port
                        "export LD_LIBRARY_PATH=~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}~%"
                        lib-path)
                ;; Point Vulkan ICD loader at Mesa's Intel driver.
                (format port
                        "export VK_ICD_FILENAMES=~a/share/vulkan/icd.d/intel_icd.x86_64.json~%"
                        #$mesa)
                (format port "exec ~a/bitwig-studio/bitwig-studio \"$@\"~%"
                        #$bitwig-studio)))
            (chmod script #o755)))))
    (synopsis "Bitwig Studio launcher with pinned Mesa/Vulkan/X11 libraries")))

bitwig-studio-wrapped
