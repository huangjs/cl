(in-package :cs325-user)
(use-package :net.aserve)
(use-package :net.html.generator)

;;; Update history:
;;;
;;; 01-15-07 Added PATHNAME-HOST and PATHNAME-DEVICE to *BASE-DIR* [CKR]
;;; 01-26-05 Added :PRESERVE to FIX-NAME [CKR]
;;; 01-25-05 Added a lot of comments [CKR]
;;; 01-23-05 Added PUBLISH-DIRECTORY, made more use of static HTML and CSS [CKR]
;;; 01-22-05 Fixed Critic page to not run #. and show error messages [CKR]
;;; 01-22-05 Fixed Apropos page to handle missing package [CKR]
;;; 02-16-04 Replaced *cs325-root* with *load-pathname* [CKR]


;;; Some AllegroServe (AServe hereafter) examples, with extended
;;; comments. These comments are not a replacement for the AServe
;;; documentation. The code is intended to exemplify recommended
;;; practice for maintainable code. Server code can be quite
;;; tricky to debug because each request is handled in a separate
;;; thread, not in the listener. You want as much of your code as
;;; possible to be testable in a listener window.


;;; Publishing URL's
;;; ----------------

;;; In Java servlets, you map URL's to programs in a web.xml file.
;;; In AServe, you link URL's to Lisp functions in a Lisp file,
;;; like this, using calls to "publish" functions.  I recommend
;;; putting all the mappings up front, to make it easy to see
;;; what URL's are defined,

;;; First, we define what AServe calls "exact" paths. If
;;; a browser asks for a URL exactly matching one of the
;;; paths below, the associated code will be called.
;;;
;;; The simplest path, "/", will match a request for
;;; http://localhost:8000/ (if 8000 is the port used).
;;; Hence, this is a good path for a page describing
;;; the server itself.

(publish :path "/" :content-type "text/html" :function 'show-root-page)

;;; The remaining URL's are for specific web applications
;;; that are described later in this file. These mappings
;;; say that, for example, the URL http://localhost:8000/apropos
;;; will cause the function SHOW-APROPOS to be called.

(publish :path "/apropos" :content-type "text/html" :function 'show-apropos)
(publish :path "/critic" :function 'critique-code)
(publish :path "/display" :function 'show-fields)

;;; We also want to make available static content,
;;; such as HTML files, images, and stylesheets. In general,
;;; the more you can put in static content, the better, because
;;; such content can be edited and tested by non-Lisp programmers
;;; using HTML editors.
;;;
;;; Although you can publish files one by one, using PUBLISH-FILE,
;;; it's simpler to make a directory with the content, and publish
;;; the entire directory.
;;;
;;; Accompanying this file should be a subdirectory called contents/.
;;; To publish that directory, we don't want to hardwire the full
;;; path, because then every one using this file would need to modify
;;; it for their installation.

;;; Instead, we construct the pathname by adding "contents/" to the
;;; name of the directory containing this file. We store the result
;;; in the global *BASE-DIR* in case you want to see what it looks
;;; like in a listener window.

(defparameter *base-dir* 
  (namestring
   (merge-pathnames "content/"
                    (make-pathname 
                     :device (pathname-device *load-truename*)
                     :host (pathname-host *load-truename*)
                     :directory (pathname-directory *load-truename*)))))

;;; Now we tell AServe that any URL starting with
;;; http://localhost:8000/base/... should retrieve files from
;;; the directory contents/.
;;;
;;; In AServe /base/ is called a "prefix" path. If, and only if,
;;; no "exact" path matches the incoming URL, then AServe checks
;;; the prefix paths. If more than one matches, it picks the longest.
;;;
;;; I deliberately used a prefix different from the directory
;;; name to demonstrate that the two are independent.
;;; Normally, however, you'd probably use the same names.
;;;
;;; CAUTIONS: Be sure that
;;;   - the prefix begins and ends with /
;;;   - the destination is a directory ending with /, e.g., "webapps/"
;;;   - the destination is a string, not a pathname structure

(publish-directory :prefix "/base/" :destination *base-dir*
                   :indexes '("index.html"))

;;; The :indexes keyword tells AServe to send back
;;; index.html from the destination directory if the URL
;;; doesn't specify a file, e.g., the URL is just
;;; http://localhost:8000/base/. The default value for
;;; this keyword is the list '("index.html" "index.htm).
;;;
;;; Typically, index pages are used as home pages for a web
;;; site. In our case, index.html has links to the
;;; demo applications.
;;;
;;; NOTE: portableaserve with LispWorks 4.2 Personal Edition 
;;; for Windows does not send index.html on my machine.
;;; However the URL http://localhost:8000/base/index.html
;;; works fine.


;;; Defining Lisp URL handlers
;;; ---------------------------

;;; Generating a Server Root Page in Lisp
;;; -------------------------------------
;;;
;;; As noted above, the URL http://localhost:8000/
;;; will call the Lisp function SHOW-ROOT-PAGE.
;;;
;;; It's often useful to have a page for the server itself.
;;; Tomcat for example has a page about the Tomcat server and
;;; links to documentation that displays at the server root.
;;;
;;; Type http://localhost:8000/ into your browser. Then use
;;; your browser's "show page source" menu option to see
;;; what the browser sees. Note that there's no Lisp there,
;;; just regular HTML.

;;; All functions that handle HTTP requests take two parameters:
;;;   - a request (as in Java), containing the form data and 
;;;     other input information,
;;;   - a entity (similar to Java's response object), containing
;;;     the stream to write HTML output to
;;;
;;; Such functions should start with with-http-response and
;;; with-http-body, as shown below. These macros do some basic
;;; HTTP processing, and set things up so that output from
;;; the HTML macro will be sent back to the client that requested
;;; the URL.

(defvar *hit-counter* 0)

(defun show-root-page (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (make-root-page (header-slot-value req :host)))))

;;; I recommend that you put all the page-generating code in a
;;; separate function. Pass that function any form data it needs,
;;; not the request or entity. That way, you'll be able to test it in a
;;; listener window, like this:
;;;
;;;   (html-stream *standard-output* (make-root-page "my-host"))
;;;
;;; This function uses the HTML macro to construct HTML text
;;; with a little bit about the server. Anything can be used
;;; to print, as long as the output goes to *HTML-STREAM*. But
;;; the HTML macro is easier to maintain in the long haul.

(defun make-root-page (host)
  (html
   (:html
    (:head
     (:title "Welcome to Portable AllegroServe on "
             (:princ (lisp-implementation-type)))
     ((:link :rel "stylesheet" :type "text/css"
             :href "base/style.css"))
     )
    (:body (:center ((:img :src "base/images/aservelogo.gif")))
           
           ;;; Note the links to static content via base/ URL's.
           
           (:h1 "Welcome to Portable AllegroServe on "
                (:princ (lisp-implementation-type)))
           
           (:i "This server's host name is "
               (:princ-safe host))
           
           #+unix
           (:i ", running on process " (:princ (net.aserve::getpid)))
           :br
           
           (:princ (incf *hit-counter*)) " hits so far in this run"
           
           ;;; Hit counters used to be popular, when being popular
           ;;; was popular on the web. A hit counter is a
           ;;; simple example of something that can only be done using
           ;;; some kind of dynamically generated HTML.
           
           ))))
  

;;; Static Form Example
;;; -------------------
;;;
;;; Forms are the heart of web applications. We have a
;;; simple, and silly, static form in contents/fields.html,
;;; which can be viewed with the URL
;;;
;;;   http://locahost:8000/base/fields.html
;;;
;;; It's meant to show some common form elements and how
;;; the data a user enters into a form can be sent to and
;;; processed by a Lisp function.
;;;
;;; The form has
;;;
;;;  - a hidden input field called "email"
;;;  - a text field called "name"
;;;  - a pair of radio buttons name "agrees"
;;;  - a checkbox named "spam" and a checkbox named "calls"
;;;  - three input fields called "favorites"
;;;
;;; Radio buttons that go together always have the same name. 
;;; Clicking one such button will uncheck all the others with
;;; the same name. But any input element can have the same name
;;; as another, and we do it here for demonstration purposes.
;;;
;;; Forms specify a URL to send to the server when the form is
;;; is submitted. In fields.html, the form is defined thus:
;;;
;;;   <form method="post" action="/display">
;;;
;;; so submitting the form sends the URL /display to the server,
;;; along with whatever data has been entered into the form fields.
;;; That URL has been defined above to call SHOW-FIELDS.

(defun show-fields (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (make-fields-page (request-query req)))))

;;; REQUEST-QUERY returns an alist of all form data:
;;;
;;;   ((name . value) (name . value) ...).
;;;
;;; so we use a simple DOLIST to create a table row
;;; for each pair.

(defun make-fields-page (alist)
  (html 
   (:html
    (:head (:title "Form Data"))
    (:body
     ((:table :border 1)
      (:tr
       (:th "Field") (:th "Value"))
      (dolist (entry alist)
        (html 
         (:tr 
          (:td (:princ (car entry)))
          (:td (:prin1 (cdr entry)))))))))))


;;; Lisp Critic Example
;;; ----------------------
;;;
;;; The URL http://localhost:8000/critic calls
;;; the Lisp function CRITIQUE-CODE. It reads Lisp code
;;; from a form, applies the Lisp Critiquer to that code,
;;; and creates an HTML page with the results.
;;;
;;; CRITIQUE-CODE implements a fairly common pattern in
;;; web pages that act like program interfaces: it's a form
;;; that submits to itself. That is, when the user submits
;;; the form, they get the form back, plus additional information.
;;; This makes it easy to modify what's in the form and resubmit.
;;;
;;; Programming such forms is a little tricky, because you 
;;; have to handle both the first time the page is shown,
;;; when there is no form data, and subsequent times when
;;; form data has been sent, and needs to be placed in the
;;; form.
;;;
;;; This code also shows two instances of the "wrapper" design
;;; pattern. A wrapper is a small function that makes another
;;; function usable in some context by translating input and output
;;; from one format to another. Wrappers are important for 
;;; keep coding small and modular. In this case,
;;;
;;;   - CRITIQUE-CODE-STRING wraps around CRITIQUE-DEFINITION,
;;;     converting a function that takes Lisp lists and prints,
;;;     into one that takes and returns strings.
;;;
;;;   - CRITIQUE-CODE wraps around CRITIQUE-CODE-STRING, adding
;;;     HTML formatting to the string returned by the inner
;;;     function.

(defun critique-code (req ent)    
  (with-http-response (req ent)
    (with-http-body (req ent)
      (make-critique-page (request-query-value "code" req)))))

(defun make-critique-page (text)
  (html 
   (:html
    (:head
     (:title "Lisp Critiquer"))
    (:body
     (:h1 "Lisp Critiquer")
     ((:form :action "/critic" :method "POST")
      ((:table :border 1)
       (:tr (:th "Enter Code in Text Area"))
       (:tr
        (:td
         ((:textarea :name "code" :rows 5 :cols 60)
          (unless (null text) (html (:princ text))))))
       
       ;; We put the code text into the form's textarea
       ;; named "code," unless this is the first entry
       ;; to the form and text is NIL. This way, the user
       ;; sees both the code and the critique, and can 
       ;; modify the code and re-critique it.
       
       (:tr
        (:th ((:input :type "submit" :value "Click to See Critiques"))))
       (:tr (:th "Critiques"))
       (:tr
        (:td
         (unless (null text)
           (html (:pre (:princ-safe (critique-code-string text)))))))
       
       ;; Here we call CRITIQUE-CODE-STRING to do the real work. It
       ;; doesn't know about HTML. It just takes a string with code and
       ;; returns a string with critiques.
       
       ))))))

;;; (CRITIQUE-CODE-STRING string) => value as string
;;;   Reads and critiques the first expression in string, returns
;;;   the output in a string.
;;;
;;; It calls CRITIQUE-DEFINITION from the lisp-critic package
;;; to do the real work. There are three important techniques
;;; used here;
;;;
;;;   - capturing the output from CRITIQUE-DEFINITION into a string
;;;
;;;   - catching any errors from CRITIQUE-DEFINITION into a string
;;;
;;;   - preventing end-users from running malicious Lisp code
;;;     on the server
;;;
;;; Capturing output: Since CRITIQUE-DEFINITION prints
;;; its results, rather than returning them, CRITIQUE-CODE-STRING
;;; uses WITH-OUTPUT-TO-STRING to redirect all output to an internal 
;;; string stream. By naming this stream *STANDARD-OUTPUT* we 
;;; temporarily make normal output go to that string stream.
;;; WITH-OUTPUT-TO-STRING returns the string constructed when
;;; it finishes.
;;;
;;; Catching errors: The Lisp special form HANDLER-CASE makes
;;; it very easy to catch any errors that occur while running
;;; code. All we do here is catch the error and return it as
;;; as a string to be displayed.
;;;
;;; Preventing malicious code: Since this code does not EVAL
;;; the user's code, you might think you're safe from malicious
;;; inputs. But you're not. By default, the Lisp reader will
;;; evaluate any expression preceded by #. -- i.e., 
;;; (READ-FROM-STRING "#.(+ 2 3)" returns 5, not (+ 2 3).
;;; Imagine reading "#.(delete-file ...)" !!
;;;
;;; Fortunately, you can turn this off by setting *READ-EVAL*
;;; to NIL. ALWAYS do this if you do any kind of Lisp READ on user
;;; input. It's safe to call READ-CHAR, however.


(defun critique-code-string (s)
  (let ((*read-eval* nil))
    (handler-case
        (with-output-to-string (*standard-output*)
          (critique-definition (read-from-string s)))
      (error (condition) (format nil "~A" condition)))))
      


;;; Lisp Apropos Example
;;; --------------------
;;; 
;;; The URL http://localhost:8000/apropos calls
;;; the Lisp function SHOW-APROPOS. It reads two fields:
;;; symbol and package, and returns the results of
;;; doing a Lisp APROPOS-LIST call with that data.
;;; It is another example of a form that submits
;;; to itself, and has another example of a wrapper
;;; function.
;;;
;;; I adapted this code from portableaserve/aserve/example.cl,
;;;
;;;  - adding a field for package
;;;  - adding output to display the value of variable if bound
;;;  - breaking the large code up into more manageable pieces
;;;  - getting rid of non-standard IF*
;;;  - dropping the my-td macro
;;;  - replacing deprecated HTML with CSS styles

(defun show-apropos (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (make-apropos-page (request-query-value "symbol" req)
                         (request-query-value "package" req)))))

;;; Note the use of strings to split the CSS style information
;;; over separate lines, and the use of subfunctions to handle
;;; generating the small form and the results table.
(defun make-apropos-page (symbol-name package-name)
  (html (:head
         (:title "Allegro Apropos") :newline
         ((:style :type "text/css") :newline
          "body { background-image: url('/base/images/fresh.jpg'); }" :newline
          "table { cell-padding: 3px; }" :newline
          "td { background-color: silver; color: black; }" :newline
          "th { background-color: blue; color: white; }"
          ))
        (:body
         (apropos-form symbol-name package-name)
         :newline
         :hr
         (unless (null symbol-name)
           (apropos-results (or symbol-name "example")
                            package-name)))))


(defun apropos-form (symbol-name package-name)
  (html 
   ((:form :action "/apropos"
     :method "get")
    (:p
     (:b "Symbol: ")
     ((:input :type "text" :size 20 :name "symbol" :if* symbol-name :value symbol-name))
     (:b "Package (optional): ")
     ((:input :type "text" :size 20 :name "package" :if* package-name :value package-name))
     ((:input :type "submit" :value "Search"))
     ))))

;;; The only messy bit here is that the package name can be left
;;; empty, in which case we want to do apropos over all packages,
;;; using NIL. But we also want to display a message if a bad
;;; package name was given.
;;;
;;; Exercise for the reader: replace the package name field with
;;; a drop-down menu of all available packages.

(defun apropos-results (symbol-name package-name)
  (let ((package (find-package (fix-name package-name))))
    (if (and (null package) (not (string= package-name "")))
        (html
         (:b "There is no package called " (:princ-safe package-name)))
      (let ((symbols (apropos-list (fix-name symbol-name) package)))
        (html 
         "Number of symbols found: " (:princ (length symbols))
         :br
         :newline
         (unless (null symbols)
           (apropos-table symbols (or package (find-package :common-lisp)))))))))

;;; This would be simpler if we didn't have to worry about
;;; Allegro "modern" mode.
(defun fix-name (text)
  (ecase (readtable-case *readtable*)
    (:upcase (string-upcase text))
    (:downcase (string-downcase text))
    (:preserve text)))


(defun apropos-table (symbols *package*)
  (html ((:table :border 3)
         (:tr (:th "Symbol") (:th "Function?") (:th "Value"))
         (dolist (symbol symbols)
           (html (:tr 
                  (:td (:prin1-safe symbol))
                  (:td (:princ (if (fboundp symbol) "Yes" "No")))
                  (:td (if (boundp symbol)
                           (html (:prin1-safe (symbol-value symbol)))
                         (html (:em "unbound"))))
                  )
                 :newline)))
        :newline))

