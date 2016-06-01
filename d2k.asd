(in-package #:asdf)

(defsystem "demon-second-kind"
  :description "Demon of the second kind, yielding text from entropy source. Inspired by Stanislaw Lem's _Cyberiad_"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :version "0.2"
  :license "BSD"
  :depends-on ("split-sequence" "btrie")
  :components ((:file "package")
	       (:file "d2k" :depends-on ("package"))))
