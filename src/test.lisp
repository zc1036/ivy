
(in-package :cl-user)

(require 'asdf)

;; from http://cl-cookbook.sourceforge.net/os.html
(defun portable-getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))


(defun portable-argv ()
  (or
   #+clisp (ext:argv)
   #+sbcl sb-ext:*posix-argv*
   #+abcl ext:*command-line-argument-list*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   (error "Common Lisp implementation not supported")))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:initialize-source-registry
 `(:source-registry
    (:directory ,(cadr (portable-argv)))
     :inherit-configuration))

(asdf:oos 'asdf:load-op 'ivy)

(defun main (argv)
  (let ((sources nil)
        (target-name "C"))
    (pop argv)
    (loop while argv do
         (let ((arg (car argv)))
           (cond
             ((string= arg "--target")
              (pop argv)
              (setf target-name (car argv)))
             (t
              (push arg sources))))
         (pop argv))

    (when (> (length sources) 1)
      (error "Cannot compile more than one source file per program invocation"))

    (ivy-hll:compile-file (car sources) target-name)))

(main (cdr (portable-argv)))
