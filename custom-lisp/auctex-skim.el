;;; AUCTex plugin into skim

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;; start emacs in server mode so that skim can talk to it
(setq server-use-tcp t)
(server-start)


(provide 'auctex-skim)
